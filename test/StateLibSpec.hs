module StateLibSpec where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List.NonEmpty as L
import qualified Data.Set.NonEmpty as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import StateLib


instance Arbitrary UserId where
  arbitrary = UserId <$> arbitrary

instance (Arbitrary a) => Arbitrary (L.NonEmpty a) where
  arbitrary = sized $ \n -> do
    k <- choose (1, n + 1)
    L.fromList <$> sequence [arbitrary | _ <- [1..k]]

instance (Ord a, Arbitrary a) => Arbitrary (S.NESet a) where
  arbitrary = S.fromList <$> arbitrary


spec :: Spec
spec = describe "All StateLib functions" $ do

  it "saveFollows only" $
    let initialFollowers = M.empty
        computation = do
          _ <- saveFollow (UserId 4) (UserId 1)
          _ <- saveFollow (UserId 3) (UserId 1)
          _ <- saveFollow (UserId 2) (UserId 1)
          return ()
        user1Followers = S.fromList $ L.fromList [UserId 2, UserId 3, UserId 4]
        finalState = M.fromList [(UserId 1, user1Followers)]
    in runState computation initialFollowers `shouldBe` ((), finalState)

  prop "saveFollow' returns Followers Map with `user` key and value containing `follower`)" $
    \follower user fs ->
      let newFs = saveFollow' follower user fs
          newUserFollowersMaybe = M.lookup user newFs
      in maybe False (S.member follower) newUserFollowersMaybe

  -- prop "saveUnfollow' returns Followers Map either w/o `user` key or with with `user` key and value not containing `follower`)" $
  --   \follower user fs -> -- M.member user fs && follower /= user ==>
  --     let newFs = saveUnfollow' follower user fs
  --         newUserFollowersMaybe = M.lookup user newFs
  --     in maybe False (S.notMember follower) newUserFollowersMaybe
