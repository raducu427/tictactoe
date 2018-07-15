{-# LANGUAGE TemplateHaskell #-}

import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Console.ANSI (clearScreen, hideCursor, cursorUpLine)
import Data.Matrix 
import qualified Data.Vector as V (sum, generate)
import Data.Char (toLower)
import Data.List (all)
import Control.Lens 
import Control.Monad.State
import Control.Monad 

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

empty   = 2
cursor  = 1 
playerX = 11
playerO = 17 
draw    = 3
playing = 4 
n       = 3  -- matrix dimension

initMatrix = setElem cursor (1,1) (matrix n n $ \(i,j) -> empty) 
initGame = Game initMatrix empty (1,1) playerX playing 

checkGameStatus :: CursorPosition -> Matrix Int -> Player -> GameStatus
checkGameStatus pos matrix currentPlayer = 
  let i = pos^._1
      j = pos^._2
      m = matrix
      antiDiagonal m = V.generate n $ \i -> m ! (i + 1, n - i)
      antiTrace m = V.sum (antiDiagonal m)
      r = (V.sum (getRow i m) * V.sum (getCol j m) * trace m * antiTrace m) `mod` currentPlayer 
  in if r == 0 then currentPlayer else 
     if all (\e -> e > empty) (toList matrix) then draw 
     else playing     
       
switchPlayer :: Player -> Player
switchPlayer currentPlayer = if currentPlayer == playerX then playerO else playerX 

transition :: Game -> Char -> Either Game Game
transition game0 c = 
    let game1 = execState (move c) game0
    in if (game1^.gameStatus) == playing then Right game1 else Left game1 

moveCursor :: Direction -> CursorPosition -> CursorPosition
moveCursor dir pos = case dir of
  MoveRight -> (pos^._1, pos^._2 `mod` n + 1)
  MoveLeft  -> (pos^._1, (pos^._2 + (-1)*(n + 2)) `mod` n + 1) 
  MoveDown  -> (pos^._1 `mod` n + 1, pos^._2)      
  MoveUp    -> ((pos^._1 + (-1)*(n + 2)) `mod` n + 1, pos^._2)        

move :: Char -> State Game () 
move key elem = do
  matrix        <- use gameMatrix
  pos           <- use position
  currentPlayer <- use player 
  prevElem      <- use prevElement  
  let newRightPos = moveCursor MoveRight pos
      newLeftPos  = moveCursor MoveLeft  pos
      newDownPos  = moveCursor MoveDown  pos 
      newUpPos    = moveCursor MoveUp    pos 
      newMatrix   = setElem currentPlayer pos matrix 
  case (toLower key) of
    'd'  -> do prevElement .= getElem (newRightPos^._1) (newRightPos^._2) matrix
               gameMatrix  %= (setElem cursor newRightPos)
               gameMatrix  %= (setElem prevElem pos)  
               position    .= newRightPos
    'a'  -> do prevElement .= getElem (newLeftPos^._1)  (newLeftPos^._2)  matrix
               gameMatrix  %= (setElem cursor newLeftPos)
               gameMatrix  %= (setElem prevElem pos)  
               position    .= newLeftPos     
    's'  -> do prevElement .= getElem (newDownPos^._1)  (newDownPos^._2)  matrix
               gameMatrix  %= (setElem cursor newDownPos)
               gameMatrix  %= (setElem prevElem pos)  
               position    .= newDownPos
    'w'  -> do prevElement .= getElem (newUpPos^._1)    (newUpPos^._2)    matrix
               gameMatrix  %= (setElem cursor newUpPos)
               gameMatrix  %= (setElem prevElem pos)  
               position    .= newUpPos 
    'p'  -> if prevElem < playerX then do 
               gameMatrix  .= newMatrix 
               gameStatus  .= checkGameStatus pos newMatrix currentPlayer
               prevElement .= currentPlayer
               player      %= switchPlayer               
            else return ()             
    otherwise -> return ()               
                                                   
printGrid :: (Matrix Int) -> IO ()
printGrid = printLists . toLists 
  where
    printLists []     = return ()
    printLists (l:[]) = printList l
    printLists (l:ls) = do {printList l; printDashes (2*(length l) - 1); printLists ls}
    printList []      = return ()
    printList (e:[])  = do {prinContent e; putChar '\n'}
    printList (e:es)  = do {prinContent e; putChar '|'; printList es}    
    printDashes n     = do {replicateM_ n  (putChar '-'); putChar '\n'}
    prinContent e 
      | e == playerX = putChar 'X'
      | e == playerO = putChar 'O'
      | e == cursor  = putChar '*'
      | e == empty   = putChar ' '    
               
render :: Either Game Game -> IO ()
render (Right game) = do 
  clearScreen
  hideCursor
  if (game^.player) == playerX then do
    printGrid (game^.gameMatrix)
    replicateM_ 3 (putChar '\n')
    putStr "player's X turn"
  else do
    printGrid (game^.gameMatrix)
    replicateM_ 3 (putChar '\n')
    putStr "player's O turn"          
  cursorUpLine 8  
render (Left game) = do    
  clearScreen
  hideCursor     
  if (game^.gameStatus) == playerX then do
    printGrid (game^.gameMatrix)
    replicateM_ 3 (putChar '\n')
    putStr "player X won"
  else if (game^.gameStatus) == playerO then do 
    printGrid (game^.gameMatrix)
    replicateM_ 3 (putChar '\n')
    putStr "player O won"
  else do  
    printGrid (game^.gameMatrix)
    replicateM_ 3 (putChar '\n')
    putStr "draw"
  cursorUpLine 8
      
input :: IO Char
input = do
    c <- getCharNoBuffering
    return c  
            
play :: Either Game Game -> IO ()
play es = do
  return initGetCharNoBuffering
  case es of
    Right s   -> do 
      render es
      c <- input 
      play $ transition s c
    otherwise -> do
      render es
      return ()      
 
main = play $ Right initGame
