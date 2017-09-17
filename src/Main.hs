module Main where

import System.Environment (getArgs)
import Data.Either (isRight, rights, lefts)

import Parser (parseDefinition)
import Compiler (compile)
import Types ( Matcher(Raw) )

main :: IO ()
main = do
    -- examples
    -- thisis = atLeastOneOf(10Times("abc"))
    -- hello1 = atLeastOneOf(10Times("abc")) or ("def" or "qwe")
    -- foobar = atLeastOneOf(10Times("abc")) + ("def" or "qwe")
    args <- getArgs
    case args of
        [filename] -> do
            rawProg <- readFile filename
            let commands = filter (/= "") . lines $ rawProg
            let raw_env = map parseDefinition commands

            if (all isRight raw_env)
            then do
                let env = rights raw_env ++ [("digit", Raw "\\d"), ("letter", Raw "[a-zA-Z]")]
                -- text <- getLine
                case (lookup "main" env) of
                    Just m -> do
                        print m
                        putStrLn $ compile env m
                    Nothing -> error "No main matcher"
            else do
                print $ lefts raw_env
                error "error parsing commands"

        _ -> putStrLn "incorrect number of command line arguements"
