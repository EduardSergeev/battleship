

module Battleship.Board where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM



type Coordinates = (Int , Int)


data Ship = Ship {
      shipType :: Type,
      bow :: Coordinates,
      placement :: Placement } deriving Show

data Type =
    Patrol |
    Cruiser |
    Submarine |
    Battleship |
    Carrier deriving (Show)

data Placement =
    Vertical |
    Horizontal deriving Show


data Status = Intact | Hit deriving Show

data Square = Square {
      status :: Status,
      ship :: ShipIndex } deriving Show

type ShipIndex = IM.Key

type Squares = M.Map Coordinates Square

data Board = Board {
      squares :: Squares,
      ships :: IM.IntMap Ship } deriving Show


minCoord = 0
maxCoord = 9


size :: Type -> Int
size Patrol = 1
size Cruiser = 2
size Submarine = 3
size Battleship = 4
size Carrier = 5


location :: Ship -> [Coordinates]
location (Ship t (x,y) p) =
    case p of
      Vertical -> [(x,y') | y' <- [y..y+size t]]
      Horizontal -> [(x',y) | x' <- [x..x+size t]]


empty :: Board
empty = Board M.empty IM.empty



insert :: Ship -> Board -> Board
insert s@(Ship t xy p) (Board sqs shs) =
    let i = IM.size shs
        sqs' = foldl' (\m xy -> M.insert xy (Square Intact i) m) sqs ls 
        shs' = IM.insert i s shs
    in if isValid then Board sqs' shs' else error "Invalid ship placement"
    where
      ls = location s
      isValid = all check ls
      check xy@(x,y) = x <= maxCoord && y <= maxCoord && M.notMember xy sqs


--      check xy@(x,y)
--            | x < minCoord || x > maxCoord = error "Invalid coordinates: " + show xy 
