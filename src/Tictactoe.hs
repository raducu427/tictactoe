{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Tictactoe where

import System.Console.ANSI ( clearScreen, hideCursor, cursorUpLine )
import Data.Matrix 
import qualified Data.Vector as V 
import Data.Char (toLower)
import Control.Lens ( (^.), (.=), (%=), use, _1, _2, makeLenses )
import Control.Monad.State

data Status    = PlayerXwon | PlayerOwon | Draw     | Playing
data Direction = MoveRight  | MoveLeft   | MoveDown | MoveUp
data Game      = Game { _gameMatrix :: Matrix Int
                      , _prevElement :: Int  
                      , _position :: (Int, Int)
                      , _player :: Int
                      , _status :: Status
                      } 
                 
makeLenses ''Game

empty    =  2
cursor   =  1 
playerX  = 11
playerO  = 17 
initGame = Game (setElem cursor (1,1) $ matrix 3 3 $ \(i,j) -> empty) empty (1,1) playerX Playing      

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
    status      .= checkGameStatus pos newMatrix currentPlayer
    prevElement .= currentPlayer
    player      %= switchPlayer               
  else return ()    
   where
     moveCursor dir pos = case dir of
       MoveRight -> (pos^._1,       pos^._2 `mod` 3 + 1)
       MoveLeft  -> (pos^._1, (pos^._2 - 5) `mod` 3 + 1) 
       MoveDown  -> (pos^._1 `mod` 3 + 1,       pos^._2)      
       MoveUp    -> ((pos^._1 - 5) `mod` 3 + 1, pos^._2) 
     checkGameStatus pos matrix currentPlayer = 
       let antiDiagonal matrix = V.generate 3 $ \i -> matrix!(i + 1, 3 - i)
           antiTrace matrix = V.sum (antiDiagonal matrix)
       in if (V.sum (getRow (pos^._1) matrix) * V.sum (getCol (pos^._2) matrix) * trace matrix * antiTrace matrix) `mod` currentPlayer == 0 
          then winner currentPlayer else if all (> empty) (toList matrix) then Draw else Playing 
     winner player = if player == playerX then PlayerXwon else PlayerOwon  
     switchPlayer currentPlayer = if currentPlayer == playerX then playerO else playerX      

render :: Game -> IO ()
render game = do  
  clearScreen
  hideCursor
  printGrid (game^.gameMatrix)
  replicateM_ 3 (putChar '\n')
  case game^.status of
    Playing    -> (if (game^.player) == playerX then putStr "player's X turn" else putStr "player's O turn") >> cursorUpLine 8
    PlayerXwon -> putStr "player X won"      
    PlayerOwon -> putStr "player O won" 
    Draw       -> putStr "draw" 
   where
     printGrid = zipWithM_ zipper [1..] . toList 
     zipper i e    = prinContent e >> if i `mod` 3 == 0 then putChar '\n' >> printDashes 5 else putChar '|'        
     printDashes n = replicateM_ n (putChar '-') >> putChar '\n'   
     prinContent e | e == playerX = putChar 'X'
                   | e == playerO = putChar 'O'
                   | e == cursor  = putChar '*'
                   | e == empty   = putChar ' '      
       
play :: Game -> IO ()
play game = case game^.status of
    Playing   -> render game >> getChar >>= \c -> play $ execState (move c) game 
    otherwise -> render game
      


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  