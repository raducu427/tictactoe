{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE TemplateHaskell  #-}
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Console.ANSI (clearScreen, hideCursor, cursorUpLine)
import Data.Matrix 
import qualified Data.Vector as V 
import Data.Char (toLower)
import Control.Lens ( (^.), (.=), (%=), use, _1, _2, makeLenses )
import Control.Monad.State
import Data.Functor.Foldable ( apo )
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as CMTF

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
  pos           <- use position
  matrix        <- use gameMatrix
  prevElem      <- use prevElement 
  currentPlayer <- use player 
  let c         = toLower key
      newMatrix = setElem currentPlayer pos matrix
      newPos c | c == 'd' = moveCursor MoveRight
               | c == 'a' = moveCursor MoveLeft
               | c == 's' = moveCursor MoveDown
               | c == 'w' = moveCursor MoveUp 
  if c `elem` ['d','a','s','w'] then do
    prevElement .= uncurry getElem (newPos c pos) matrix     
    gameMatrix  %= (setElem cursor (newPos c pos))
    gameMatrix  %= (setElem prevElem pos)  
    position    .= newPos c pos   
  else if c == 'p' && prevElem < min playerX playerO then do 
    gameMatrix  .= newMatrix 
    status      .= checkGameStatus pos newMatrix currentPlayer
    prevElement .= currentPlayer
    player      %= (\currentPlayer -> if currentPlayer == playerX then playerO else playerX)               
  else return ()    
   where
     moveCursor dir pos = case dir of
       MoveRight -> (fst pos,       snd pos `mod` 3 + 1)  
       MoveLeft  -> (fst pos, (snd pos - 5) `mod` 3 + 1) 
       MoveDown  -> (fst pos `mod` 3 + 1,       snd pos)      
       MoveUp    -> ((fst pos - 5) `mod` 3 + 1, snd pos) 
     checkGameStatus pos matrix currentPlayer = 
       let antiDiagonal matrix = V.generate 3 $ \i -> matrix!(i + 1, 3 - i)
           antiTrace matrix = V.sum (antiDiagonal matrix)
       in if (V.sum (getRow (fst pos) matrix) * V.sum (getCol (snd pos) matrix) * trace matrix * antiTrace matrix) `mod` currentPlayer == 0 
          then (\player -> if player == playerX then PlayerXwon else PlayerOwon ) currentPlayer 
          else if all (> empty) (toList matrix) then Draw else Playing      

render :: Game -> IO ()
render game = do  
  clearScreen >> hideCursor >> printGrid (game^.gameMatrix) >> replicateM_ 3 (putChar '\n')
  case game^.status of
    Playing    -> (if (game^.player) == playerX then putStr "player's X turn" else putStr "player's O turn") >> cursorUpLine 8
    PlayerXwon -> putStr "player X won"      
    PlayerOwon -> putStr "player O won" 
    Draw       -> putStr "draw" 
   where
     printGrid                    = zipWithM_ zipper [1..] . toList 
     zipper i e                   = prinContent e >> if i `mod` 3 == 0 then putChar '\n' >> printDashes 5 else putChar '|'        
     printDashes n                = replicateM_ n (putChar '-') >> putChar '\n'   
     prinContent e | e == playerX = putChar 'X'
                   | e == playerO = putChar 'O'
                   | e == cursor  = putChar '*'
                   | e == empty   = putChar ' '     
        
data TerminalF a = TerminalF Game (Char -> a) deriving Functor 

play :: Game -> Free TerminalF ()
play = apo coalg where
  coalg game = CMTF.Free $ TerminalF game $ \c -> case game^.status of 
    Playing   -> Right $ execState (move c) game 
    otherwise -> Left  $ Pure () 

run :: Free TerminalF r -> IO () 
run (Pure                  r) = return ()
run (Free (TerminalF game f)) = render game >> getCharNoBuffering >>= run . f        
    
main = return initGetCharNoBuffering >> run $ play initGame
