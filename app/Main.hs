module Main where

import Parser (parseStringToBf)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parseStringToBf line of
    Right tokens -> print tokens
    Left e -> print e
  main
