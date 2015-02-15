
module BoardTest
(
  tests
) where

import Control.Monad
import Control.Applicative hiding (empty)

import Battleship.Board

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Arbitrary
import System.Random
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


instance Arbitrary Coordinates where
    arbitrary = (-|-) <$> choose (minX,maxX) <*> choose (minY,maxY)


instance Arbitrary Ship where
    arbitrary = arbitraryBoundedEnum >>= arbitraryShip


instance Arbitrary Board where
    arbitrary = foldM add empty [Patrol .. Carrier]
        where
          add b t = do
            sh <- arbitraryShip t `suchThat` shipCanBeAdded b
            return $ addShip b sh


arbitraryShip :: ShipType -> Gen Ship
arbitraryShip t = do
      p <- arbitraryBoundedEnum
      let s = shipSize t
          mx = toEnum $ fromEnum maxX - s
          my = maxY - s
          (xr,yr) = case p of
                      Vertical -> ((minX,maxX), (minY,my)) 
                      Horizontal -> ((minX,mx), (minY,maxY))
      xy <- (-|-) <$> choose xr <*> choose yr
      return $ newShip t xy p


prop_ShipCanBeAddedToEmptyBoard :: Ship -> Bool
prop_ShipCanBeAddedToEmptyBoard sh =
    shipCanBeAdded empty sh
    
    


tests = [
 testGroup "Creation" [
                testProperty "AddShip to empty Board"  prop_ShipCanBeAddedToEmptyBoard
               ]
 ]