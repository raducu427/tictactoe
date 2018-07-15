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
      r = (V.sum (getRow (pos^._1) matrix) * V.sum (getCol (pos^._2) matrix) * trace matrix * antiTrace matrix) `mod` currentPlayer 
  in if r == 0 then currentPlayer else 
     if all (\e -> e > empty) (toList matrix) then draw 
     else playing     
       
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
printGrid = printLists . toLists where
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
      if      (game^.gameStatus) == playerX then putStr "player X won"
      else if (game^.gameStatus) == playerO then putStr "player O won"
                                            else putStr "draw"
            
play :: Either Game Game -> IO ()
play eitherGame = do
  return initGetCharNoBuffering
  case eitherGame of
    Right game -> do 
      render eitherGame
      c <- getCharNoBuffering 
      play $ transition game c
    otherwise -> do
      render eitherGame
      return ()        
 
main = play $ Right initGame
