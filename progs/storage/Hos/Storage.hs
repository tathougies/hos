module Hos.Storage where

import Control.Monad

import Data.Word
import Data.Elf

import Hos.User.SysCall
import Hos.Common.Types

import Foreign.Ptr
import Foreign.Storable

import Numeric

loadElf :: String -> Ptr Elf64Hdr -> Word64 -> Word64 -> IO ()
loadElf name vStart physStart physSize =
    do (elfHdr, progHdrs) <- elf64ProgHdrs vStart
       aRef <- hosEmptyAddressSpace
       forM_ progHdrs $ \progHdr ->
           case ph64Type progHdr of
             PtLoad -> hosAddMapping aRef (ph64VAddr progHdr) (ph64VAddr progHdr + ph64MemSz progHdr) (CopyOnWrite (UserSpace ReadWrite) (ph64Offset progHdr + fromIntegral physStart))
             _ -> return 0
       r <- hosFork
       case r of
         0 -> hosEnterAddressSpace aRef (e64Entry elfHdr)
         childId -> do hosDebugLog ("[storage] started " ++ name ++ " as " ++ show childId)
                       hosCloseAddressSpace aRef
                       hosYield

