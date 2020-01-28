module Main where

import System.Environment (getArgs)

import Types (CompileError(..))
import Compiler (compileProg)
import Util (matchText)


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
            case (compileProg rawProg mainName) of
                Right regex -> do
                    if export
                    then putStrLn regex
                    else do
                        text <- getLine
                        case (matchText regex text "\x1b[102m" "\x1b[0m") of
                            Nothing -> putStrLn "no match"
                            Just highlightedString -> putStrLn highlightedString
                Left NoMainMatcher -> error "No main matcher"
                Left (ParseErrors errors) -> print errors >> error "error parsing commands"
                Left CyclicDefn -> error "Cyclic definitions are not allowed"
        _ -> putStrLn "incorrect number of command line arguements"
