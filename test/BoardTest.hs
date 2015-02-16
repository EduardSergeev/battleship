
module BoardTest
(
  tests
) where

import Data.List

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Common

import Battleship.Board


prop_ShipCanBeAddedToEmptyBoard :: Ship -> Bool
prop_ShipCanBeAddedToEmptyBoard sh = shipCanBeAdded empty sh

prop_OverlappingShips :: Ship -> Ship -> Property
prop_OverlappingShips sh1 sh2 =
    not (null (shipCoordinates sh1 `intersect` shipCoordinates sh2)) ==>
        let b = addShip empty sh1
        in not (shipCanBeAdded b sh2)


prop_ShipInitiallyIntact :: Ship -> Bool
prop_ShipInitiallyIntact sh =
    let b = addShip empty sh
    in all (==IntactShip) . map (b!) . shipCoordinates $ sh

prop_ShipHitOnce :: Ship -> Int -> Bool
prop_ShipHitOnce sh i =
    let b = addShip empty sh
        xys = shipCoordinates sh
        hxy = xys !! (i `mod` length xys)
        xys' = delete hxy xys 
        (r,b') = attack b hxy
    in r == Hit &&
       b' ! hxy == HitShip &&
       (all (==IntactShip) . map (b'!) $ xys')
  
prop_ShipBadlyHitButNotSunk :: Ship -> Int -> Bool
prop_ShipBadlyHitButNotSunk sh i =
    let b = addShip empty sh
        xys = shipCoordinates sh
        ixy = xys !! (i `mod` length xys)
        xys' = delete ixy xys
        (rs,b') = runAttacks b xys'
    in all (==Hit) rs &&
       b' ! ixy == IntactShip &&
       (all (==HitShip) . map (b'!) $ xys')

prop_ShipIsSunk :: Ship -> Bool
prop_ShipIsSunk sh =
    let b = addShip empty sh
        xys = shipCoordinates sh
        ((lr:rs),b') = runAttacks b xys
    in all (==Hit) rs &&
       lr == Sunk &&
       (all (==SunkShip) . map (b'!) $ xys)

prop_CheckAllSunkSingle :: Ship -> Bool
prop_CheckAllSunkSingle sh =
    let b = addShip empty sh
        xys = shipCoordinates sh
        (_,b') = runAttacks b xys
    in not (allSunk b) && allSunk b'

prop_CheckAllSunk :: Board -> Bool
prop_CheckAllSunk b =
    let (_,b') = runAttacks b entireBoard
    in not (allSunk b) && allSunk b'


prop_InitialBoard :: Board -> Bool
prop_InitialBoard b =
    all ((`elem`[Water, IntactShip]) . (b!)) entireBoard

prop_GameOverBoard :: Board -> Bool
prop_GameOverBoard b =
    let (_,b') = runAttacks b entireBoard
    in all ((`elem`[Water, SunkShip]) . (b'!)) entireBoard


runAttacks b xys = foldl' step ([],b) xys
    where step (rs,b) xy = let (r,b') = attack b xy in (r:rs,b')


tests = [
    testGroup "Creation" [
        testProperty "AddShip to empty Board"      prop_ShipCanBeAddedToEmptyBoard,
        testProperty "Check overlapping ships"     prop_OverlappingShips
    ],
    testGroup "Ship Lifetime" [
        testProperty "Initialy completely intact"  prop_ShipInitiallyIntact,
        testProperty "First hit"                   prop_ShipHitOnce,
        testProperty "Last square standing"        prop_ShipBadlyHitButNotSunk,
        testProperty "Is finally Sunk"             prop_ShipIsSunk
    ],
    testGroup "Querying" [
        testProperty "AllSunk with single ship"    prop_CheckAllSunkSingle,
        testProperty "AllSunk for sure"            prop_CheckAllSunk,
        testProperty "Initial board view"          prop_InitialBoard,
        testProperty "All sunk board view"         prop_GameOverBoard
    ],
    testGroup "Graphics" [
    ]
 ]