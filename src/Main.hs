module Main where

import System.Environment (getArgs)
import Data.Either (isRight, rights, lefts)
import Text.Regex.PCRE

import Parser (parseDefinition)
import Compiler (compile, initialEnv)


highlightString :: String -> [(MatchOffset, MatchLength)] -> String
highlightString text = highlightString' (zip text [0..])
    where
    highlightString' :: [(Char, Int)] -> [(MatchOffset, MatchLength)] -> String
    highlightString' [] _ = []
    highlightString' ((c, pos):xs) ranges@((i, len):ys)
        | pos == i = "\x1b[102m" ++ c:(highlightString' xs ((i, len):ys)) -- start of highlight range
        | pos == i + len - 1 = c:"\x1b[0m" ++ (highlightString' xs ys) -- end of highlight range
        | otherwise = c:(highlightString' xs ranges) -- inside or outside a highlight range
    highlightString' input [] = map fst input

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
            let mainName = takeWhile (/= '.') filename
            let commands = filter (/= "") . lines $ rawProg
            let raw_env = map parseDefinition commands

            if (all isRight raw_env)
            then do
                let env = rights raw_env ++ initialEnv
                case (lookup mainName env) of
                    Just m -> do
                        let regex = compile env m
                        if export
                        then putStrLn regex
                        else do
                            text <- getLine
                            let matches = getAllMatches ((text =~ regex) :: AllMatches [] (MatchOffset, MatchLength))
                            case matches of
                                [] -> putStrLn "no match"
                                _ -> putStrLn $ highlightString text matches
                    Nothing -> error "No main matcher"
            else do
                print $ lefts raw_env
                error "error parsing commands"

        _ -> putStrLn "incorrect number of command line arguements"
