module Hos.Privileges where
import Hos.Types

import Data.Monoid
import qualified Data.Set as S

initPrivileges :: Privileges
initPrivileges = Privileges
              { canCreateAddressSpace = True
              , canModifyAddressSpace = Just fullAddressSpace
              , canAddFromPhysicalMapping = Just fullAddressSpace
              , canReplaceAddressSpaces = S.singleton (TaskId 0)
              , canKill = S.singleton (TaskId 0)
              , canGrantPrivileges = True
              , canRevokePrivileges = True }

noPrivileges :: Privileges
noPrivileges = Privileges
             { canCreateAddressSpace = False
             , canModifyAddressSpace = Nothing
             , canAddFromPhysicalMapping = Nothing
             , canReplaceAddressSpaces = S.empty
             , canKill = S.empty
             , canGrantPrivileges = False
             , canRevokePrivileges = False }

-- There is a strict ordering on privilege sets. A privilege set A is
-- less than or equal to a privilege set B if every privilege in A is
-- also in B
instance Ord Privileges where
    a <= b = checkImplies canCreateAddressSpace &&
             checkImplies canGrantPrivileges &&
             checkImplies canRevokePrivileges &&
             checkAddressRange canModifyAddressSpace &&
             checkAddressRange canAddFromPhysicalMapping &&
             checkSubset canReplaceAddressSpaces
        where
          -- Verify the implication f b => f a
          checkImplies f = if f a then f b else True

          -- Verify that the closed interval f b is a subset of f a
          checkAddressRange f = case (f b, f a) of
                                  (Nothing, _) -> False
                                  (_, Nothing) -> True
                                  (Just (AR bStart bEnd), Just (AR aStart aEnd)) ->
                                      bStart <= aStart && bEnd >= aEnd

          -- Verify that f b is a subset of f a
          checkSubset f = f a `S.isSubsetOf` f b

-- Privilege sets form a monoid.
instance Monoid Privileges where
    mempty = noPrivileges
    mappend a b = Privileges
                { canCreateAddressSpace = canCreateAddressSpace a || canCreateAddressSpace b
                , canModifyAddressSpace = combineRanges
                                          (canModifyAddressSpace a)
                                          (canModifyAddressSpace b)
                , canAddFromPhysicalMapping = combineRanges
                                              (canAddFromPhysicalMapping a)
                                              (canAddFromPhysicalMapping b)
                , canReplaceAddressSpaces = S.union
                                            (canReplaceAddressSpaces a)
                                            (canReplaceAddressSpaces b)
                , canGrantPrivileges = canGrantPrivileges a || canGrantPrivileges b
                , canRevokePrivileges = canRevokePrivileges a || canRevokePrivileges b }
        where combineRanges Nothing x = x
              combineRanges x Nothing = x
              combineRanges (Just (AR aStart aEnd)) (Just (AR bStart bEnd)) =
                  Just (AR (min aStart bStart) (max aEnd bEnd))

canKillP :: TaskId -> Privileges
canKillP taskId = mempty { canKill = S.singleton taskId }

canReplaceAddressSpaceP :: TaskId -> Privileges
canReplaceAddressSpaceP taskId = mempty { canReplaceAddressSpaces = S.singleton taskId }
