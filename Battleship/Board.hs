
module Battleship.Board
(
   Coordinates,
   ShipType(..),
   Placement(..),
   Ship,
   AttackResult(..),
   Board,

   minX, maxX, minY, maxY,

   (-|-),
   newShip,

   shipSize,

   empty,
   shipCanBeAdded,
   addShip,
   generateBoard,

   attack,

   allSunk,

   showSquare,
   showBoard
) where

import Data.List (foldl')
import System.Random
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM


data Coordinates = Char :|: Int  deriving (Eq, Ord)

instance Bounded Coordinates where
    minBound = (minX :|: minY)
    maxBound = (maxX :|: maxY)

instance Show Coordinates where
    show (x :|: y) = x : "-|-" ++ show y
    

data Ship = Ship {
      shipType :: ShipType,
      bow :: Coordinates,
      placement :: Placement } deriving Show

data ShipType =
    Patrol |
    Cruiser |
    Submarine |
    Battleship |
    Carrier deriving (Bounded, Enum, Show)

data Placement =
    Vertical |
    Horizontal deriving (Bounded, Enum, Show)

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


data AttackResult = Miss | Hit | Sunk | Duplicate deriving Show



minX = 'a'
maxX = 'j'
minY = 1
maxY = 10


(-|-) :: Char -> Int -> Coordinates
x -|- y =
    let xy = x :|: y
    in if validCoordinates xy then xy else error "Out of range coordinates"

validCoordinates (x :|: y) =
    x >= minX && x <= maxX && y >= minY && y <= maxY


newShip :: ShipType -> Coordinates -> Placement -> Ship
newShip t xy p =
    let sh = Ship t xy p
        xys = shipCoords sh
    in if validShip sh then sh else error "Invalid ship placement"

shipSize :: ShipType -> Int
shipSize = succ . fromEnum

shipCoords (Ship t (x:|:y) p) =
    case p of
      Vertical -> map (x:|:) [y .. y + shipSize t]
      Horizontal -> map ((:|:y) . toEnum) [fromEnum x .. fromEnum x + shipSize t]

validShip = all validCoordinates . shipCoords

empty :: Board
empty = Board M.empty IM.empty


shipCanBeAdded :: Board -> Ship -> Bool
shipCanBeAdded (Board sqs shs) sh = all (`M.notMember`sqs) . shipCoords $ sh

addShip :: Board -> Ship -> Board
addShip b@(Board sqs shs) sh =
    let i = IM.size shs
        sqs' = foldl' (\m xy -> M.insert xy (Square False i) m) sqs (shipCoords sh) 
        shs' = IM.insert i sh shs
    in if shipCanBeAdded b sh then Board sqs' shs' else error "Overlaps with another ship"

generateBoard :: RandomGen g => g -> Board
generateBoard g = snd $ foldl' addRandomShip (g,empty) [Patrol .. Carrier]
  where
    addRandomShip (g,b) t = head
        [ (g', addShip b sh) |
          ((xy,p),g') <- tail $ iterate (genBow . snd) (error "gg", g),
          let sh = Ship t xy p,
          validShip sh && shipCanBeAdded b sh]
    genBow g = 
        let (x, g') = randomR (minX,maxX) g
            (y, g'') = randomR (minY,maxY) g'
            (p, g''') = random g''
        in ((x:|:y, if p then Vertical else Horizontal), g''')



attack :: Board -> Coordinates -> (AttackResult, Board)
attack b@(Board sqs shs) xy =
    case M.lookup xy sqs of
      Nothing -> (Miss, b)
      Just (Square _ i) ->
          let sqs' = M.insert xy (Square True i) sqs
              sh = shs IM.! i
              res = if isSunk sh sqs' then Sunk else Hit
          in (res, Board sqs' shs)


isSunk sh sqs = all attacked [sqs M.! xy | xy <- shipCoords sh]

allSunk :: Board -> Bool
allSunk = all attacked . M.elems . squares


showBoard :: (Coordinates -> Char) -> String
showBoard f = unlines $
    (' ' : xs) :
    [ last (show y) : [f (x:|:y) | x <- xs] ++ "|"  | y <- ys] ++
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
