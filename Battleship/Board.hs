
module Battleship.Board
(
  Coordinates,
  ShipType(..),
  Placement(..),
  Ship,
  AttackResult(..),
  SquareContent(..),
  Board,

  -- * Coordinate functions
  (-|-),
  column,
  row,

  -- * Ship functions
  newShip,
  shipSize,
  shipCoordinates,

  -- * Board creation and checks
  empty,
  generateBoard,
  shipCanBeAdded,
  addShip,

  -- * Fun stuff
  attack,

  -- * Querying functions
  (!),
  allSunk,

  -- * Graphics
  showSquare,
  showBoard,
  showBoardWith,
) where

import Data.List (foldl')
import System.Random
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

-- | Coordinates to be used with the board
data Coordinates = Char :|: Int  deriving (Eq, Ord)


instance Bounded Coordinates where
    minBound = (minX :|: minY)
    maxBound = (maxX :|: maxY)

instance Show Coordinates where
    show (x :|: y) = x : "-|-" ++ show y
    
-- | Individual ship data
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

-- | There are two ways to place a ship on the 'Board'
data Placement =
    Vertical |
    Horizontal deriving (Bounded, Enum, Show)

data Square = Square {
      attacked :: Bool,
      ship :: ShipIndex } deriving Show

type ShipIndex = IM.Key

type Squares = M.Map Coordinates Square

-- | Player's own game board
--   All 'Ships' are visible and enemy's attacks are recorded
data Board = Board {
      squares :: Squares,
      ships :: IM.IntMap Ship }

instance Show Board where
    show = showBoard True

-- | Result of enemy's attack
data AttackResult =
    Miss |
    Hit |
    Sunk |
    Duplicate deriving (Eq, Show)

-- | Current content of the board square
--   Is constantly updated as the enemy proceed with their attacks
--   Once a give 'Ship' is sunk all its squares are marked as 'SunkShip'
data SquareContent =
    Water |
    IntactShip |
    HitShip |
    SunkShip deriving (Eq, Show)


minX = 'a'
maxX = 'j'
minY = 1
maxY = 10


infix 9 -|-
-- | Smart constructor of the 'Coordinates'
--   Checks that the resulting 'Coordinates' are within 'Board' limits
(-|-) :: Char -> Int -> Coordinates
x -|- y =
    let xy = x :|: y
    in if validCoordinates xy then xy else error "Out of range coordinates"

column :: Coordinates -> Char
column (x:|:_) = x

row :: Coordinates -> Int
row (_:|:y) = y

validCoordinates (x :|: y) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

-- | Smart constructor of the 'Ship'
--   Checks that the 'Ship' fits within the 'Board' limits
newShip :: ShipType -> Coordinates -> Placement -> Ship
newShip t xy p =
    let sh = Ship t xy p
        xys = shipCoordinates sh
    in if validShip sh then sh else error "Invalid ship placement"

shipSize :: ShipType -> Int
shipSize = succ . fromEnum

-- | Return the list of the coordinates a given 'Ship' consists of
shipCoordinates :: Ship -> [Coordinates]
shipCoordinates (Ship t (x:|:y) p) =
    case p of
      Vertical -> map (x:|:) [y .. y + shipSize t]
      Horizontal -> map ((:|:y) . toEnum) [fromEnum x .. fromEnum x + shipSize t]

validShip = all validCoordinates . shipCoordinates

-- | Create empty (no ships) board
--   Ships later can be added with 'addShip' function
empty :: Board
empty = Board M.empty IM.empty

-- | Check that a given 'Ship' does not overlap with the ships already places on the 'Board'
shipCanBeAdded :: Board -> Ship -> Bool
shipCanBeAdded (Board sqs shs) sh = all (`M.notMember`sqs) . shipCoordinates $ sh

-- | Add 'Ship' to the 'Board'
--   An error is thrown in case adding ship would overlap with exising ships
addShip :: Board -> Ship -> Board
addShip b@(Board sqs shs) sh =
    let i = IM.size shs
        sqs' = foldl' (\m xy -> M.insert xy (Square False i) m) sqs (shipCoordinates sh) 
        shs' = IM.insert i sh shs
    in if shipCanBeAdded b sh then Board sqs' shs' else error "Overlaps with another ship"

-- | Generate the full board (5 ships) using given 'Random' generator
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


-- | Enemy attack on playe's board
--   Returns the result of the attack plus updated board
--   Duplicate attacks have no effect
attack :: Board -> Coordinates -> (AttackResult, Board)
attack b@(Board sqs shs) xy =
    case M.lookup xy sqs of
      Nothing -> (Miss, b)
      Just (Square _ i) ->
          let sqs' = M.insert xy (Square True i) sqs
              sh = shs IM.! i
              res = if isSunk sh sqs' then Sunk else Hit
          in (res, Board sqs' shs)

infixl 8 !
-- | Get the content of the square of the 'Board' at a given 'Coordinates'
(!) :: Board -> Coordinates -> SquareContent
(!) (Board sqs shs) xy = 
    case M.lookup xy sqs of
      Nothing -> Water
      Just (Square { attacked = False }) -> IntactShip
      Just (Square { ship = i }) ->
          if isSunk (shs IM.! i) sqs then SunkShip else HitShip

-- | Check if all player's ships are already sunk
--   Which means the player has lost the game
allSunk :: Board -> Bool
allSunk = all attacked . M.elems . squares

isSunk sh sqs = all attacked [sqs M.! xy | xy <- shipCoordinates sh]

-- | String representation of the 'Board'
--   Where passed function is called for every square to get its 'Char' representation
--   With or without 'Coordinates' side-frames (see 'Bool' parameter)
showBoardWith :: (Coordinates -> Char) -> Bool -> String
showBoardWith f sc = unlines . concat $ [
    [' ' : xs] `sel` [],
    [ (show (y `mod` 10) `sel` "") ++ [f (x:|:y) | x <- xs] ++ ("|" `sel` "")  | y <- ys],
    [' ' : map (const '-') xs] `sel` []]
    where
      xs = [minX..maxX]
      ys = [minY..maxY]
      sel l r = if sc then l else r
      
showSquare :: Board -> Coordinates -> Char
showSquare b xy =
    case b ! xy of
      Water -> ' '
      IntactShip -> '#'
      HitShip -> 'X'
      SunkShip -> 'o'

showBoard :: Bool -> Board -> String
showBoard sc b = showBoardWith (showSquare b) sc