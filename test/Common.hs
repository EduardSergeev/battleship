module Common
(
  entireBoard
) where

import Control.Monad
import Control.Applicative hiding (empty)

import Test.QuickCheck

import Battleship.Board


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

minCoord = minBound :: Coordinates
maxCoord = maxBound :: Coordinates

entireBoard = [x-|-y | x <- [minX..maxX], y <- [minY..maxY]]

minX = column minCoord
maxX = column maxCoord
minY = row minCoord
maxY = row maxCoord

