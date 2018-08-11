module Compiler where

import Data.List (intersperse)
import Data.Either (isRight, rights, lefts)

import Parser (parseDefinition)
import Types

compileProg :: String -> String -> Either CompileError String
compileProg prog mainName
    | all isRight rawEnv = case (lookup mainName env) of
        Just m -> Right (compile env m)
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
compile env (Var str) = case (lookup str env) of
    Just m -> compile env m
    Nothing -> error ("undeclared variable " ++ str ++ " used")

escape :: Char -> String
escape '(' = "\\("
escape ')' = "\\)"
escape '.' = "\\."
escape '\\' = "\\\\"
escape c = [c]
