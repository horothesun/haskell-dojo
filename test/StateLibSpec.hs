module StateLibSpec where

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NL
import qualified Data.Set.NonEmpty as NS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import StateLib


instance Arbitrary UserId where
  arbitrary = UserId <$> arbitrary

instance (Arbitrary a) => Arbitrary (NL.NonEmpty a) where
  arbitrary = sized $ \n -> do
    k <- choose (1, n + 1)
    NL.fromList <$> sequence [arbitrary | _ <- [1..k]]

instance (Ord a, Arbitrary a) => Arbitrary (NS.NESet a) where
  arbitrary = NS.fromList <$> arbitrary


findInNESet :: Ord a => a -> NS.NESet a -> Maybe a
findInNESet x xs = if NS.member x xs then Just x else Nothing

isNESetSingleton :: NS.NESet a -> Bool
isNESetSingleton = (== 1) . NS.size


spec :: Spec
spec = describe "All StateLib functions" $ do

  it "saveFollows only" $
    let initialFollowers = M.empty
        computation = do
          _ <- saveFollow (UserId 4) (UserId 1)
          _ <- saveFollow (UserId 3) (UserId 1)
          _ <- saveFollow (UserId 2) (UserId 1)
          return ()
        user1Followers = NS.fromList $ NL.fromList [UserId 2, UserId 3, UserId 4]
        finalState = M.fromList [(UserId 1, user1Followers)]
    in runState computation initialFollowers `shouldBe` ((), finalState)

  it "mix of saveFollows and saveUnfollow" $
    let initialFollowers = M.empty
        computation = do
          _ <- saveFollow (UserId 4) (UserId 1)
          _ <- saveFollow (UserId 3) (UserId 1)
          _ <- saveFollow (UserId 2) (UserId 1)
          _ <- saveUnfollow (UserId 3) (UserId 1)
          _ <- saveFollow (UserId 5) (UserId 2)
          return ()
        user1Followers = NS.fromList $ NL.fromList [UserId 2, UserId 4]
        user2Followers = NS.singleton $ UserId 5
        finalState = M.fromList [(UserId 1, user1Followers), (UserId 2, user2Followers)]
    in runState computation initialFollowers `shouldBe` ((), finalState)

  describe "saveFollow'" $ do
    prop "returns Followers Map with `user` key and value containing `follower`" $
      \follower user fs ->
        let newFs = saveFollow' follower user fs
        in isJust $ findInNESet follower =<< M.lookup user newFs

    prop "on user with followers returns Followers Map either w/o `user` key or with `user` key and value not containing `follower`" $
      \follower user fs -> M.member user fs ==>
        let newFs = saveUnfollow' follower user fs
            newUserFollowersMaybe = M.lookup user newFs
            oldUserFollowersMaybe = M.lookup user fs
            wasOnlyFollower = maybe False isNESetSingleton oldUserFollowersMaybe
        in maybe wasOnlyFollower (NS.notMember follower) newUserFollowersMaybe

    prop "on user w/o followers returns same Followers Map" $
      \follower user fs -> M.notMember user fs ==> saveUnfollow' follower user fs == fs
