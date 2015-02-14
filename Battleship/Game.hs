

module Battleship.Game
(
  Game,

  newGame,
  
  makeShot
) where


import qualified Data.Map.Strict as M
import qualified Battleship.Board as B
import Battleship.Board(
   Board,
   Coordinates,
   AttackResult(..))


data Game = Game {
    board :: Board,
    attacks :: Attacks }

instance Show Game where
    show = showEnemyBoard

type Attacks = M.Map Coordinates AttackResult



newGame :: Board -> Game
newGame b = Game b M.empty


makeShot :: Game -> Coordinates -> (AttackResult, Game)
makeShot g@(Game b ss) xy =
    case M.lookup xy ss of
      Just r -> (r, g)
      Nothing ->
          let (r,b') = B.attack b xy
          in (r, Game b' (M.insert xy r ss)) 


showSquare :: Game -> Coordinates -> Char
showSquare (Game b ss) xy =
    case M.lookup xy ss of
      Nothing -> '?'
      Just Miss -> ' '
      _ -> B.showSquare b xy

showEnemyBoard :: Game -> String
showEnemyBoard g =
    B.showBoard (showSquare g)