module Main where

import System.IO
import Tictactoe

main :: IO ()
main = hSetBuffering stdin NoBuffering >> play initGame

