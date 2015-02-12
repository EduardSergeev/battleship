{-# LANGUAGE TupleSections #-}

module Battleship.Board where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM



type Coordinates = (Int, Int)


data Ship = Ship {
      shipType :: Type,
      bow :: Coordinates,
      placement :: Placement } deriving Show

data Type =
    Patrol |
    Cruiser |
    Submarine |
    Battleship |
    Carrier deriving (Show, Enum)

data Placement =
    Vertical |
    Horizontal deriving Show


data Status = Intact | Attacked deriving (Show, Eq)

data Square = Square {
      status :: Status,
      ship :: ShipIndex } deriving Show

type ShipIndex = IM.Key

type Squares = M.Map Coordinates Square

data Board = Board {
      squares :: Squares,
      ships :: IM.IntMap Ship } deriving Show


data AttackResult = Miss | Hit | Sunk deriving Show

minCoord = 1
maxCoord = 10


shipSize :: Type -> Int
shipSize = succ . fromEnum


shipCoords :: Ship -> [Coordinates]
shipCoords (Ship t (x,y) p) =
    case p of
      Vertical -> map (x,) [y .. y + shipSize t]
      Horizontal -> map (,y) [x .. x + shipSize t]


empty :: Board
empty = Board M.empty IM.empty

insert :: Ship -> Board -> Board
insert s@(Ship t xy p) (Board sqs shs) =
    let i = IM.size shs
        sqs' = foldl' (\m xy -> M.insert xy (Square Intact i) m) sqs xys 
        shs' = IM.insert i s shs
    in if isValid then Board sqs' shs' else error "Invalid ship placement"
    where
      xys = shipCoords s
      isValid = all check xys
      check xy@(x,y) = x <= maxCoord && y <= maxCoord && M.notMember xy sqs

fromList :: [Ship] -> Board
fromList = foldl' (flip insert) empty


attack :: Coordinates -> Board -> (AttackResult, Board)
attack xy b@(Board sqs shs) =
    case M.lookup xy sqs of
      Nothing -> (Miss, b)
      Just (Square _ i) ->
          let sqs' = M.insert xy (Square Attacked i) sqs
              sh = shs IM.! i 
              sunk = all (==Attacked) [status (sqs' M.! xy) | xy <- shipCoords sh]
          in (if sunk then Sunk else Hit, Board sqs' shs)

allSunk :: Board -> Bool
allSunk = all (\s -> status s == Attacked) . M.elems . squares