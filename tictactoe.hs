{-# LANGUAGE TemplateHaskell #-}
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Console.ANSI (clearScreen, hideCursor, cursorUpLine)
import Data.Matrix 
import qualified Data.Vector as V (sum, generate)
import Data.Char (toLower)
import Control.Lens 
import Control.Monad.State

type CursorPosition = (Int, Int)
type Player = Int
type GameStatus = Int
data Direction = MoveRight | MoveLeft | MoveDown | MoveUp
 
data Game = Game { _gameMatrix :: Matrix Int
                 , _prevElement :: Int  
                 , _position :: CursorPosition
                 , _player :: Player
                 , _gameStatus :: GameStatus
                 } 
                 
makeLenses ''Game

n        =  3  -- matrix dimension
empty    =  2
cursor   =  1 
playerX  = 11
playerO  = 17 
draw     =  3
playing  =  4 
initGame =  Game (setElem cursor (1,1) $ matrix n n $ \(i,j) -> empty) empty (1,1) playerX playing 

checkGameStatus :: CursorPosition -> Matrix Int -> Player -> GameStatus
checkGameStatus pos matrix currentPlayer = 
  let antiDiagonal matrix = V.generate n $ \i -> matrix!(i + 1, n - i)
      antiTrace matrix = V.sum (antiDiagonal matrix)
  in if (V.sum (getRow (pos^._1) matrix) * V.sum (getCol (pos^._2) matrix) * trace matrix * antiTrace matrix) `mod` currentPlayer == 0 
     then currentPlayer else if all (> empty) (toList matrix) then draw else playing     
       
switchPlayer :: Player -> Player
switchPlayer currentPlayer = if currentPlayer == playerX then playerO else playerX 

transition :: Game -> Char -> Either Game Game
transition game0 c = let game1 = execState (move c) game0
    in if (game1^.gameStatus) == playing then Right game1 else Left game1 

moveCursor :: Direction -> CursorPosition -> CursorPosition
moveCursor dir pos = case dir of
  MoveRight -> (pos^._1, pos^._2 `mod` n + 1)
  MoveLeft  -> (pos^._1, (pos^._2 + (-1)*(n + 2)) `mod` n + 1) 
  MoveDown  -> (pos^._1 `mod` n + 1, pos^._2)      
  MoveUp    -> ((pos^._1 + (-1)*(n + 2)) `mod` n + 1, pos^._2)        

move :: Char -> State Game () 
move key = do
  matrix        <- use gameMatrix
  pos           <- use position
  currentPlayer <- use player 
  prevElem      <- use prevElement 
  let newPos c | c == 'd' = moveCursor MoveRight
               | c == 'a' = moveCursor MoveLeft
               | c == 's' = moveCursor MoveDown
               | c == 'w' = moveCursor MoveUp 
      newMatrix = setElem currentPlayer pos matrix 
      c = toLower key
  if c `elem` ['d','a','s','w'] then do
    prevElement .= uncurry getElem (newPos c pos) matrix     
    gameMatrix  %= (setElem cursor (newPos c pos))
    gameMatrix  %= (setElem prevElem pos)  
    position    .= newPos c pos   
  else if c == 'p' && prevElem < playerX then do 
    gameMatrix  .= newMatrix 
    gameStatus  .= checkGameStatus pos newMatrix currentPlayer
    prevElement .= currentPlayer
    player      %= switchPlayer               
  else return ()         

printGrid :: (Matrix Int) -> IO ()
printGrid = zipWithM_ zipper [1..] . toList where
  zipper i e = do
    prinContent e
    if i `mod` 3 == 0 then do
      putChar '\n'
      printDashes $ 2 * n - 1
    else putChar '|' 
  printDashes n = replicateM_ n  (putChar '-') *> putChar '\n'
  prinContent e 
    | e == playerX = putChar 'X'
    | e == playerO = putChar 'O'
    | e == cursor  = putChar '*'
    | e == empty   = putChar ' ' 
                  
render :: Either Game Game -> IO ()
render eitherGame = do  
  clearScreen
  hideCursor
  case eitherGame of
    Right game -> do
      printGrid (game^.gameMatrix)
      replicateM_ 3 (putChar '\n')
      if (game^.player) == playerX then putStr "player's X turn" else putStr "player's O turn"          
      cursorUpLine 8 
    Left game  -> do
      printGrid (game^.gameMatrix)
      replicateM_ 3 (putChar '\n')    
      if      (game^.gameStatus) == playerX then putStrLn "player X won"
      else if (game^.gameStatus) == playerO then putStrLn "player O won" else putStrLn "draw"
            
play :: Either Game Game -> IO ()
play eitherGame = do
  case eitherGame of
    Right game -> do 
      render eitherGame
      c <- getCharNoBuffering 
      play $ transition game c
    otherwise -> do
      render eitherGame
      return ()        
 
main = return initGetCharNoBuffering <* play $ Right initGame
