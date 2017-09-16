module Main where
import Data.List (intersperse)

data Matcher = Lit String -- "abc"
             | Many Matcher -- many(...)
             | Many1 Matcher -- many1(...)
             | Matcher `Or` Matcher -- ... Or ...
             | OneOf [Matcher] -- One of: [..., ...]
             | Matcher `And` Matcher -- ... + ...
             | NTimes Int Matcher -- 3times(...)

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
  putStrLn $ compile (OneOf [])
