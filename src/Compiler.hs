module Compiler (compileProg) where

import Data.List (intersperse)
import Data.Either (isRight, rights, lefts)
import Data.Maybe (isJust, fromJust)
import Control.Arrow (second, (&&&))

import Parser (parseDefinition)
import Types
import Util (filterMap)

-- type Graph = (String, [String])

getChildren :: Matcher -> [String]
getChildren (Lit _) = []
getChildren (Many m) = getChildren m
getChildren (Many1 m) = getChildren m
getChildren (Or m1 m2) = getChildren m1 ++ getChildren m2
getChildren (OneOf ms) = concatMap getChildren ms
getChildren (And m1 m2) = getChildren m1 ++ getChildren m2
getChildren (NTimes _ m) = getChildren m
getChildren (Var s) = [s]
getChildren (Optional m) = getChildren m
getChildren (Raw _) = []

defnToGraph :: Definition -> (String, [String])
defnToGraph (name, m) = (name, getChildren m)

isValid :: [Definition] -> Matcher -> String -> Bool
isValid env m name = isDag (map defnToGraph env) [] (defnToGraph (name, m))

isDag :: [(String, [String])] -> [String] -> (String, [String]) -> Bool
isDag env visited (name, children)
  | name `elem` visited = False
  | otherwise = all (isDag env (name:visited)) nodesPointedTo
  where
  nodesPointedTo = filterMap (isJust . snd) (second fromJust) . map (id &&& (`lookup` env)) $ children

compileProg :: String -> String -> Either CompileError String
compileProg prog mainName
    | all isRight rawEnv = case lookup mainName env of
        Just m -> if isValid env m mainName then Right (compile env m) else Left CyclicDefn
        Nothing -> Left NoMainMatcher
    | otherwise = Left . ParseErrors $ lefts rawEnv
    where
    commands = filter (/= "") . lines $ prog
    rawEnv = map parseDefinition commands
    env = rights rawEnv ++ initialEnv

initialEnv :: [Definition]
initialEnv = [("digit", Raw "\\d"), ("letter", Raw "[a-zA-Z]"), ("char", Raw "\\w")]

capture :: String -> String
capture s = "(" ++ s ++ ")"

cnc :: [Definition] -> Matcher -> String
cnc env = capture . compile env

compile :: [Definition] -> Matcher -> String
compile _   (Lit str) = concatMap escape str
compile _   (Raw regex) = regex
compile env (Many m) = (cnc env m) ++ "*"
compile env (Many1 m) = (cnc env m) ++ "+"
compile env (m1 `Or` m2) = (cnc env m1) ++ "|" ++ (cnc env m2)
compile env (OneOf matchers) = concat . intersperse "|" . map (cnc env) $ matchers
compile env (m1 `And` m2) = (cnc env m1) ++ (cnc env m2)
compile env (NTimes n m) = (cnc env m) ++ "{" ++ show n ++ "}"
compile env (Optional m) = (cnc env m) ++ "?"
compile env (Var str) = case lookup str env of
    Just m -> compile env m
    Nothing -> error ("undeclared variable " ++ str ++ " used")
    -- FIXME: instead of calling error here, should instead return an Either CompileError String
    -- so an error can be shown to the user

escape :: Char -> String
escape '(' = "\\("
escape ')' = "\\)"
escape '.' = "\\."
escape '\\' = "\\\\"
escape c = [c]
