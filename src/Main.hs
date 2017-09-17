module Main where

import Parser (parseDefinition)
import Compiler (compile)

main :: IO ()
main = do
  -- examples
  -- thisis = atLeastOneOf(10Times("abc"))
  -- hello1 = atLeastOneOf(10Times("abc")) or ("def" or "qwe")
  -- foobar = atLeastOneOf(10Times("abc")) + ("def" or "qwe")
  command <- getLine
  case (parseDefinition command) of
    Right (name, m) -> do
      putStrLn name
      print m
      putStrLn $ compile m
    Left err -> print err
