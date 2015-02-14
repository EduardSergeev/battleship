{-# LANGUAGE TupleSections #-}

module Battleship.Board
(
   Coordinates,
   ShipType(..),
   Placement(..),
   Ship(..),
   AttackResult(..),
   Board,

   empty,
   addShip,

   attack,

   isSunk,
   allSunk,

   showSquare,
   showBoard
) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM



type Coordinates = (Char, Int)


data Ship = Ship {
      shipType :: ShipType,
      bow :: Coordinates,
      placement :: Placement } deriving Show

data ShipType =
    Patrol |
    Cruiser |
    Submarine |
    Battleship |
    Carrier deriving (Show, Enum)

data Placement =
    Vertical |
    Horizontal deriving Show

data Square = Square {
      attacked :: Bool,
      ship :: ShipIndex } deriving Show

type ShipIndex = IM.Key

type Squares = M.Map Coordinates Square


data Board = Board {
      squares :: Squares,
      ships :: IM.IntMap Ship }

instance Show Board where
    show = showFullBoard


data AttackResult = Miss | Hit | Sunk deriving Show




minX = 'a'
maxX = 'j'
minY = 1
maxY = 10


shipSize :: ShipType -> Int
shipSize = succ . fromEnum


shipCoords :: Ship -> [Coordinates]
shipCoords (Ship t (x,y) p) =
    case p of
      Vertical -> map (x,) [y .. y + shipSize t]
      Horizontal -> map ((,y) . toEnum) [fromEnum x .. fromEnum x + shipSize t]


empty :: Board
empty = Board M.empty IM.empty

addShip :: Board -> Ship -> Board
addShip (Board sqs shs) s@(Ship t xy p) =
    let i = IM.size shs
        sqs' = foldl' (\m xy -> M.insert xy (Square False i) m) sqs xys 
        shs' = IM.insert i s shs
    in if isValid then Board sqs' shs' else error "Invalid ship placement"
    where
      xys = shipCoords s
      isValid = all check xys
      check xy@(x,y) = x <= maxX && y <= maxY && M.notMember xy sqs


attack :: Board -> Coordinates -> (AttackResult, Board)
attack b@(Board sqs shs) xy =
    case M.lookup xy sqs of
      Nothing -> (Miss, b)
      Just (Square _ i) ->
          let sqs' = M.insert xy (Square True i) sqs
              sh = shs IM.! i
              res = if isSunk sh sqs' then Sunk else Hit
          in (res, Board sqs' shs)


isSunk :: Ship -> Squares -> Bool
isSunk sh sqs = all attacked [sqs M.! xy | xy <- shipCoords sh]

allSunk :: Board -> Bool
allSunk = all attacked . M.elems . squares


showBoard :: (Coordinates -> Char) -> String
showBoard f = unlines $
    (' ' : xs) :
    [ last (show y) : [f (x,y) | x <- xs] ++ "|"  | y <- ys] ++
    [' ' : map (const '-') xs]
    where
      xs = [minX..maxX]
      ys = [minY..maxY]

showSquare :: Board -> Coordinates -> Char
showSquare (Board sqs shs) xy =
    case M.lookup xy sqs of
      Nothing -> ' '
      Just (Square { attacked = False }) -> 'H'
      Just (Square { ship = i }) ->
          if isSunk (shs IM.! i) sqs then 'o' else 'X'


showFullBoard b = showBoard (showSquare b)
