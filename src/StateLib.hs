module StateLib where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set.NonEmpty as S

newtype UserId = UserId Int
  deriving (Eq, Ord, Show)

type Followers = M.Map UserId (S.NESet UserId)

saveFollow :: UserId -> UserId -> State Followers ()
saveFollow follower user = state $
  \fs -> ((), saveFollow' follower user fs)

saveFollow' :: UserId -> UserId -> Followers -> Followers
saveFollow' follower user fs = M.insert user newUserFollowers fs
  where
    oldUserFollowersMaybe = M.lookup user fs
    newUserFollowers = maybe (S.singleton follower) (S.insert follower) oldUserFollowersMaybe

saveUnfollow :: UserId -> UserId -> State Followers ()
saveUnfollow follower user = state $
  \fs -> ((), saveUnfollow' follower user fs)

saveUnfollow' :: UserId -> UserId -> Followers -> Followers
saveUnfollow' follower user fs = maybe fs removeFollower oldUserFollowersMaybe
  where
    oldUserFollowersMaybe = M.lookup user fs
    isOnlyFollower nes = S.size nes == 1 && S.member follower nes
    removeFollower nes = if isOnlyFollower nes
      then M.delete user fs
      else M.insert user (S.unsafeFromSet $ S.delete follower nes) fs
