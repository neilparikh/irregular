module Main where

import Data.List (intersperse)

import Types
import Parser (parseMatcher)

capture :: String -> String
capture s = "(?:" ++ s ++ ")"

cnc :: Matcher -> String
cnc = capture . compile

compile :: Matcher -> String
compile (Lit str) = str
compile (Many m) = (cnc m) ++ "*"
compile (Many1 m) = (cnc m) ++ "+"
compile (m1 `Or` m2) = (cnc m1) ++ "|" ++ (cnc m2)
compile (OneOf matchers) = concat . intersperse "|" . map cnc $ matchers
compile (m1 `And` m2) = (cnc m1) ++ (cnc m2)
compile (NTimes n m) = (cnc m) ++ "{" ++ show n ++ "}"

main :: IO ()
main = do
  case (parseMatcher "atLeastOneOf(10Times(\"Abc\"))") of
    Right m -> do
      print m
      putStrLn $ compile m
    Left _ -> putStrLn "parse error"
