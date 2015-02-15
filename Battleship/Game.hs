

module Battleship.Game
(
  -- * Game for one player
  Game,

  -- * Coordinate functions
  (-|-),

  -- * Game setup
  newGame,
  
  -- * Game play
  attack,

  -- * Game history
  history,
  attackResult,

  -- * Graphics
  showEnemyBoard
) where


import qualified Data.Map.Strict as M
import qualified Battleship.Board as B
import Battleship.Board (
   Board,
   Coordinates,
   AttackResult(..),
   (-|-))


data Game = Game {
    board :: Board,
    attacks :: Attacks,
    -- | A list of previous attack coordinates in reverse time order
    history :: [Coordinates] }

instance Show Game where
    show = showEnemyBoard

type Attacks = M.Map Coordinates AttackResult


-- | Create an empty set of boards fo one player
newGame :: Board -> Game
newGame b = Game b M.empty []

-- | Make attack move
attack :: Game -> Coordinates -> (AttackResult, Game)
attack g@(Game b ss hs) xy =
    case M.lookup xy ss of
      Just r -> (Duplicate, g)
      Nothing ->
          let (r,b') = B.attack b xy
          in (r, Game b' (M.insert xy r ss) (xy:hs)) 


attackResult :: Game -> Coordinates -> Maybe AttackResult
attackResult g xy = M.lookup xy (attacks g)


showSquare :: Game -> Coordinates -> Char
showSquare (Game b ss _) xy =
    case M.lookup xy ss of
      Nothing -> '?'
      Just Miss -> ' '
      _ -> B.showSquare b xy

showEnemyBoard :: Game -> String
showEnemyBoard g =
    B.showBoard (showSquare g)