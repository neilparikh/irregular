module Main where

import System.Environment (getArgs)
import Data.Either (isRight, rights, lefts)
import Text.Regex.PCRE
import Data.Array (elems)

import Parser (parseDefinition)
import Compiler (compile)
import Types ( Matcher(Raw) )

main :: IO ()
main = do
    -- examples
    -- thisis = atLeastOneOf(10Times("abc"))
    -- hello1 = atLeastOneOf(10Times("abc")) or ("def" or "qwe")
    -- foobar = atLeastOneOf(10Times("abc")) + ("def" or "qwe")
    raw_args <- getArgs
    let (export, args) = if length raw_args > 0 && head raw_args == "--export"
                         then (True, tail raw_args)
                         else (False, raw_args)
    case args of
        [filename] -> do
            rawProg <- readFile filename
            let commands = filter (/= "") . lines $ rawProg
            let raw_env = map parseDefinition commands

            if (all isRight raw_env)
            then do
                let env = rights raw_env ++ [ ("digit", Raw "\\d"), ("letter", Raw "[a-zA-Z]"), ("char", Raw "\\w")]
                text <- getLine
                case (lookup "main" env) of
                    Just m -> do
                        let regex = compile env m
                        if export
                        then putStrLn regex
                        else do
                            let matches = elems $ ((text =~ regex) :: MatchArray)
                            case matches of
                                [] -> putStrLn "no match"
                                ((i, j):_) -> do
                                    -- Note: this highlights is pretty bad
                                    -- only highlights the first match
                                    putStr $ take i text
                                    putStr "\x1b[102m"
                                    putStr $ take (j-i) $ drop i text
                                    putStr "\x1b[0m"
                                    putStrLn $ drop (j) text
                    Nothing -> error "No main matcher"
            else do
                print $ lefts raw_env
                error "error parsing commands"

        _ -> putStrLn "incorrect number of command line arguements"
