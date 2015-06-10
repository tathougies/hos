module Hos.IPC where

import Hos.Types
import Hos.Common.Types
import Hos.SysCall
import Hos.Memory
import Hos.CBits

import Control.Monad hiding (forM_)

import Data.Word
import Data.Bits
import Data.Monoid
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import Data.Foldable (forM_)
import qualified Data.IntervalMap as IntervalMap
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Foreign.Ptr
import Foreign.Storable

maxMessageQueueSz = 1024

-- | can be used with map.alter to insert a message into a queue
insertMessageIntoQueue :: MessageInDelivery -> Maybe (Seq MessageInDelivery) -> Maybe (Seq MessageInDelivery)
insertMessageIntoQueue messageInDelivery Nothing = Just (Seq.singleton messageInDelivery)
insertMessageIntoQueue messageInDelivery (Just msgQueue) = Just (msgQueue |> messageInDelivery)

unmapMappingsFor :: MessageType -> SysCallM r v e (Seq (Maybe Word64))
unmapMappingsFor msgType =
    do curTask <- getCurrentTask
       let mappings = addrSpaceRegions (taskAddressSpace curTask)
           messageMappings = filter (isMappingForMsgType . snd) mappings

           isMappingForMsgType (Message msgType' _) = msgType' == msgType
           isMappingForMsgType _ = False

           messagePages = mconcat $
                          map (\(_, Message _ pages) -> pages) messageMappings

           AddressSpace addrSpaceIntervals = taskAddressSpace curTask
           newCurAddressSpace = foldr IntervalMap.delete addrSpaceIntervals (map fst messageMappings)
           curTask' = curTask { taskAddressSpace = AddressSpace newCurAddressSpace }

       arch <- getArch
       -- Now, unmap the pages in the current task...
       x <- liftIO $ forM_ messageMappings $ \((start, end), _) ->
            archWalkVirtMemTbl arch (taskVirtMemTbl curTask) start (end - 1) $ \virt phys ->
              archUnmapPage arch virt

       x1 <- setCurrentTask curTask'
       x `seq` x1 `seq` return messagePages

wakeUpTask :: TaskId -> TaskId -> ChanId -> SysCallM r v e ()
wakeUpTask sleeper waker wakerChan =
    do sleeperTask <- getTask sleeper
       case taskPleaseWakeUp sleeperTask of
         NoThanks -> return ()
         FromWaitOnChannels (WaitOnChannelsFlags waitForever dontTruncate noMask) senderTaskPtr sleeperPrio
             | noMask || wakerChan `S.member` (taskUnmaskedChannels sleeperTask) ->
                 do x <- scheduleTask sleeperPrio sleeper
                    arch <- getArch
                    curTask <- getCurrentTask
                    curTask' <- liftIO $ archSwitchTasks arch curTask sleeperTask
                    (sleeperTask', wocResponse) <- completeDelivery dontTruncate sleeperTask wakerChan senderTaskPtr
                    sleeperTask'' <- liftIO $ do archReturnToUserspace arch (fromSysCallReturnable wocResponse)
                                                 archSwitchTasks arch sleeperTask' curTask'
                    x1 <- setCurrentTask curTask'
                    x `seq` x1 `seq` setTask sleeper sleeperTask''
             | otherwise -> return ()

deliverMessage :: ChanId -> MessageEndpoint -> SysCallM r v e Word8
deliverMessage sourceChan dst@(MessageEndpoint recipientTaskSpec recipientChan) =
    do -- Check if there are any physical regions mapped to the sourceChannel
       curTask <- getCurrentTask
       curTaskId <- getCurrentTaskId
       (recipient, recipientTaskId) <-
           if recipientTaskSpec == TaskId 0
              then do router <- getTask (taskRouter curTask)
                      return (router, taskRouter curTask)
              else do recipient <- getTask recipientTaskSpec
                      if curTaskId /= taskRouter recipient then throwSysCallError InsufficientPrivileges else return (recipient, recipientTaskSpec)

       -- Now we want to check that the outgoing queue for sourceChan is free (we're free to send). Otherwise, throw an error
       case M.lookup sourceChan (taskOutgoingMessages curTask) of
         Just _ -> throwSysCallError SourceChanIsFull
         Nothing ->
             case M.lookup recipientChan (taskIncomingMessages recipient) of
               Just queue
                   | Seq.length queue > maxMessageQueueSz -> throwSysCallError DestChanIsFull
               _ ->
                   do replyPages <- unmapMappingsFor (Incoming (ReplyTo sourceChan)) -- Delivery on this channel will invalidate any reply mappings
                      messagePages <- unmapMappingsFor (Outgoing (MessageFrom sourceChan))

                      curTask' <- getCurrentTask

                      let messageInDelivery = MessageInDelivery (MessageEndpoint curTaskId sourceChan) dst messagePages
                          pageCount = fromIntegral (Seq.length messagePages)
                          recipient' = recipient { taskIncomingMessages = M.alter (insertMessageIntoQueue messageInDelivery) recipientChan (taskIncomingMessages recipient) }
                          curTask'' = curTask' { taskOutgoingMessages = M.insert sourceChan messageInDelivery (taskOutgoingMessages curTask') }

                      x <- setTask recipientTaskId recipient'
                      x1 <- setCurrentTask curTask''
                      x2 <- wakeUpTask recipientTaskId curTaskId recipientChan
                      -- Now free the memory for all pages that were sent as a reply
                      arch <- getArch
                      x3 <- liftIO $ forM_ replyPages $ \page ->
                            case page of
                              Just page -> cPageAlignedPhysFree page (fromIntegral (archPageSize arch))
                              Nothing -> return ()

                      x `seq` x1 `seq` x2 `seq` x3 `seq` return pageCount

routeMessage :: ChanId -> MessageEndpoint -> SysCallM r v e Word8
routeMessage sourceChanId dst@(MessageEndpoint recipientId recipientChanId) =
    -- Basic steps:
    --   1) Ensure sourceChanId has an inProcessMessage
    --   2) Make sure the recipient's queue is not full
    --   3) remove sourceChanId from inProessMessages, and insert into recipient's  queue
    --   4) Re-schedule recipient if necessary
    do curTask <- getCurrentTask
       curTaskId <- getCurrentTaskId
       recipient <- getTask recipientId
       -- In order to be able to route messages to a task, we must be that task's router...
       if taskRouter recipient /= curTaskId then throwSysCallError InsufficientPrivileges
          else case M.lookup sourceChanId (taskInProcessMessages curTask) of
                 Nothing -> throwSysCallError ChannelIsEmpty
                 Just (MessageInDelivery source _ pages) ->
                     do let messageInDelivery = MessageInDelivery source dst pages
                        case M.lookup recipientChanId (taskIncomingMessages recipient) of
                          Just msgQueue
                              | Seq.length msgQueue > maxMessageQueueSz -> throwSysCallError DestChanIsFull
                          _ -> do let curTask' = curTask { taskInProcessMessages = M.delete sourceChanId (taskInProcessMessages curTask) }
                                      recipient' = recipient { taskIncomingMessages = M.alter (insertMessageIntoQueue messageInDelivery) recipientChanId (taskIncomingMessages recipient) }
                                  x <- setCurrentTask curTask'
                                  pages <- unmapMappingsFor (Incoming (MessageFrom sourceChanId))
                                  let pageCount = fromIntegral (Seq.length pages)

                                  x1 <- setTask recipientId recipient'
                                  x2 <- wakeUpTask recipientId curTaskId recipientChanId
                                  x `seq` x1 `seq` x2 `seq` return pageCount

replyQueueFor :: ChanId -> ChanId
replyQueueFor (ChanId x) = ChanId (x `setBit` 31)

replyToMessage :: ChanId -> SysCallM r v e Word8
replyToMessage onChanId =
    -- Steps:
    --   1) Ensure that there is a message in onChanId
    --   2) Unmap Incoming (MessageFrom onChanId) and Outgoing (ReplyTo onChanId) pages
    --   3) Deliver (Outgoing (ReplyTo onChanId)) pages to the recipient
    do curTask <- getCurrentTask
       case M.lookup onChanId (taskInProcessMessages curTask) of
         Nothing -> throwSysCallError ChannelIsEmpty
         Just (MessageInDelivery (MessageEndpoint sourceId sourceChanId) _ _) ->
           do source <- getTask sourceId

              incomingPages <- unmapMappingsFor (Incoming (MessageFrom onChanId))
              pages <- unmapMappingsFor (Outgoing (ReplyTo onChanId))

              curTask' <- getCurrentTask
              curTaskId <- getCurrentTaskId
              let messageInDelivery = MessageInDelivery (MessageEndpoint curTaskId onChanId) (MessageEndpoint sourceId (replyQueueFor sourceChanId)) pages
                  curTask'' = curTask' { taskInProcessMessages = M.delete onChanId (taskInProcessMessages curTask') }
                  source' = source { taskIncomingMessages = M.insert (replyQueueFor sourceChanId) (Seq.singleton messageInDelivery) (taskIncomingMessages source) }
                  pageCount = fromIntegral (Seq.length pages)

              x <- setCurrentTask curTask''
              x1 <- setTask sourceId source'

              x2 <- wakeUpTask sourceId curTaskId (replyQueueFor sourceChanId)
              -- Now free the memory for all pages that were sent to the channel we're replying to...
              arch <- getArch
              x3 <- liftIO $ forM_ incomingPages $ \page ->
                    case page of
                      Just page -> cPageAlignedPhysFree page (fromIntegral (archPageSize arch))
                      Nothing -> return ()

              x `seq` x1 `seq` x2 `seq` x3 `seq` return pageCount

data WaitOnChannelsResponse = WaitOnChannelsResponse
                            { wocWasTruncated :: Bool
                            , wocChanId :: ChanId }
                            | Waiting

instance SysCallReturnable WaitOnChannelsResponse where
    fromSysCallReturnable (WaitOnChannelsResponse False (ChanId chanId)) = fromIntegral chanId
    fromSysCallReturnable (WaitOnChannelsResponse True (ChanId chanId)) = fromIntegral chanId `setBit` 33
    fromSysCallReturnable Waiting = 0

completeDelivery :: Bool -> Task r v -> ChanId -> Ptr TaskId -> SysCallM r v e (Task r v, WaitOnChannelsResponse)
completeDelivery dontTruncate curTask chanId@(ChanId chanIdRaw) retSenderPtr =
    -- Steps:
    --   1) Find the message on chanId in curTask
    --   2) Check if we need to truncate to deliver the message, and if this is an issue
    --   3) Move the message to inprocessing (TODO: should we check if something is there???)
    --   4) Modify the address space to map in the pages...
    case M.lookup chanId (taskIncomingMessages curTask) of
      Nothing -> fail "compeleteDelivery: chanId must have available message"
      Just msgQueue ->
          case viewl msgQueue of
            EmptyL -> fail "completeDelivery: empty message queue in task struct"
            (msg@(MessageInDelivery (MessageEndpoint (TaskId senderTask) senderChan) _ pages )) :< msgQueue' ->
                -- Now find the mappings corresponding to this channel
                do let expMessageType = Incoming $ if testBit chanIdRaw 31 then ReplyTo (ChanId (chanIdRaw `clearBit` 31)) else MessageFrom chanId
                       messageMappings = filter (isMappingOfType . snd) $ addrSpaceRegions (taskAddressSpace curTask)
                       isMappingOfType (Message messageType _) = messageType == expMessageType
                       isMappingOfType _ = False

                       messageMappingsLength = sum $ map (\(_, Message _ pages) -> Seq.length pages) messageMappings

                   -- If we don't have enough space, and we cannot truncate, then throw an error
                   if messageMappingsLength < Seq.length pages && dontTruncate
                      then throwSysCallError WouldBeTruncated
                      else -- If we're here, we don't care about truncation, or we have enough pages...
                           do let messageMappings' = assignPagesToMappings messageMappings pages
                                  assignPagesToMappings [] _ = []
                                  assignPagesToMappings _ pages
                                      | Seq.null pages = []
                                  assignPagesToMappings (((start, end), Message msgType mappedPages):mappings) pagess =
                                      let (pages, pagess') = Seq.splitAt (Seq.length mappedPages) pagess
                                      in (start, Message msgType pages) : assignPagesToMappings mappings pagess'
                                  AddressSpace aSpace = taskAddressSpace curTask
                                  newAddressSpace = foldr (\(start, mapping) aSpace -> IntervalMap.simpleUpdateAt start mapping aSpace) aSpace messageMappings'
                                  curTask' = curTask { taskInProcessMessages = M.insert chanId msg (taskInProcessMessages curTask)
                                                     , taskIncomingMessages = (if Seq.null msgQueue' then M.delete chanId else M.insert chanId msgQueue')
                                                                              (taskIncomingMessages curTask)
                                                     -- If this message is a reply, then delete the message who is being replied to
                                                     , taskOutgoingMessages = if chanIdRaw `testBit` 31
                                                                              then M.delete (ChanId (chanIdRaw `clearBit` 31)) (taskOutgoingMessages curTask)
                                                                              else taskOutgoingMessages curTask
                                                     , taskAddressSpace = AddressSpace newAddressSpace }
--                              dbg <- scDebugLog ("complete delivery " ++ show messageMappingsLength ++ " pages (exp " ++ show expMessageType ++ "). msg is " ++ show pages)
                              x <- liftIO (poke (castPtr retSenderPtr) senderTask)
                              x `seq` return (curTask', WaitOnChannelsResponse (messageMappingsLength < Seq.length pages) chanId)

waitOnChannels :: WaitOnChannelsFlags -> Word64 -> Ptr TaskId -> SysCallM r v e WaitOnChannelsResponse
waitOnChannels flags@(WaitOnChannelsFlags waitForever dontTruncate noMask) timeout retSenderPtr =
    do -- First check if any of the unmasked channels have messages waiting on them. If so, return immediately.
       -- Otherwise, suspend for the given amount of time (or forever, if waitForever is set)

      curTask <- getCurrentTask
      curTaskId <- getCurrentTaskId
      let unmaskedAndAvailable = M.keysSet (taskIncomingMessages curTask) `S.intersection` taskUnmaskedChannels curTask
      if S.null unmaskedAndAvailable
        then if timeout == 0 && not waitForever
             then throwSysCallError NoMessagesAvailable
             else do x <- suspendTask curTaskId -- Remove ourselves from the scheduling cycle...
                     curTask' <- switchToNextTask
                     curTaskPrio <- getCurrentTaskPriority
                     let curTask'' = curTask' { taskPleaseWakeUp = FromWaitOnChannels flags retSenderPtr curTaskPrio } -- Let others know we would like to be woken up...
                     x1 <- setTask curTaskId curTask'' -- can't use setCurrentTask because we've switched out of it
                     -- TODO get timeouts working
                     -- when (not waitForever) $ scheduleWakeUp timeout
                     x `seq` x1 `seq` return Waiting
        else do let chanId = S.findMin unmaskedAndAvailable
                (curTask', res) <- completeDelivery dontTruncate curTask chanId retSenderPtr
                x <- setCurrentTask curTask'
                x `seq` return res

unmaskChannel :: ChanId -> SysCallM r v e ()
unmaskChannel chanId = do curTask <- getCurrentTask
                          let curTask' = curTask { taskUnmaskedChannels = S.insert chanId (taskUnmaskedChannels curTask) }
                          setCurrentTask curTask'
