
module Battleship.Game
(
  -- * Game for one player
  Game,
  AttackResult(..),
  SquareContent(..),

  -- * Coordinate functions
  (-|-),
  column,
  row,

  -- * Game setup
  newGame,
  generateGame,
  
  -- * Game play
  attack,

  -- * Current game state
  (!),
  isGameOver,

  -- * Game history
  history,
  attackResult,

  -- * Graphics
  showGame
) where


import System.Random
import qualified Data.Map.Strict as M
import qualified Battleship.Board as B
import Battleship.Board(
   Board,
   Coordinates,
   AttackResult(..),
   SquareContent(..),
   (-|-),
   column,
   row)

-- | Player's view of enemy's 'Board'
--   Only so far 'attack'-ed squares are visible
data Game = Game {
    board :: Board,
    attacks :: Attacks,
    -- | A list of previous attack coordinates in reverse time order
    history :: [Coordinates] }

instance Show Game where
    show = showGame True

type Attacks = M.Map Coordinates AttackResult


-- | Create new 'Game' for a given evemy's 'Board'
newGame :: Board -> Game
newGame b = Game b M.empty []

-- | Generate new 'Game' using 'Random' placements for evemy's 'Ship's
generateGame :: RandomGen g => g -> Game
generateGame g = newGame (B.generateBoard g)

-- | Make attack move
--   Returns the result of the attack plus updated 'Game'
--   This updated Game will have attacked square uncovered (with 'SquareContent')
--   plus if the 'Ship' is sunk all its squares are also marked as 'SunkShip'
attack :: Game -> Coordinates -> (AttackResult, Game)
attack g@(Game b as hs) xy =
    case M.lookup xy as of
      Just r -> (Duplicate, g)
      Nothing ->
          let (r,b') = B.attack b xy
          in (r, Game b' (M.insert xy r as) (xy:hs)) 


infixl 8 !
-- | Get the content of enemy's board at a given 'Coordinates'.
--   'Nothing' if the square has not been 'attack'-ed yet
(!) :: Game -> Coordinates -> Maybe SquareContent
(!) (Game b as _) xy = 
    if M.member xy as then Just (b B.! xy) else Nothing

-- | Check if the current game is won by the player
--   (i.e. all enemy's ships are sunk)
isGameOver :: Game -> Bool
isGameOver g = B.allSunk . board $ g

-- | The result of the previously executed attack.
--   (historic view)
attackResult :: Game -> Coordinates -> Maybe AttackResult
attackResult g xy = M.lookup xy (attacks g)


showSquare :: Game -> Coordinates -> Char
showSquare g xy =
    case attackResult g xy of
      Nothing -> '?'
      Just Miss -> ' '
      _ -> B.showSquare (board g) xy

showGame :: Bool -> Game -> String
showGame sc g =
    B.showBoardWith (showSquare g) sc