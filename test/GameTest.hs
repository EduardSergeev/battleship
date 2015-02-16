
module GameTest
(
  tests
) where

import Data.List
import Data.Maybe
import Control.Applicative

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Common

import Battleship.Game



instance Arbitrary Game where
    arbitrary = newGame <$> arbitrary


prop_InitialBoard :: Game -> Bool
prop_InitialBoard g =
    all ((==Nothing) . (g!)) entireBoard

prop_GameOverBoard :: Game -> Bool
prop_GameOverBoard g =
    let (_,g') = runAttacks g entireBoard
        rs = map (fromJust . (g'!)) entireBoard
    in all (`elem`[Water, SunkShip]) rs


runAttacks b xys = foldl' step ([],b) xys
    where step (rs,b) xy = let (r,b') = attack b xy in (r:rs,b')


tests = [
    testGroup "Creation" [
    ],
    testGroup "Querying" [
        testProperty "Initial board view"          prop_InitialBoard,
        testProperty "All sunk board view"         prop_GameOverBoard
    ]
 ]
