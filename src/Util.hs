module Util where

import Text.Regex.PCRE

highlightString :: String -> String -> String -> [(MatchOffset, MatchLength)] -> String
highlightString text left right = highlightString' (zip text [0..])
    where
    highlightString' :: [(Char, Int)] -> [(MatchOffset, MatchLength)] -> String
    highlightString' [] _ = []
    highlightString' ((c, pos):xs) ranges@((i, len):ys)
        | pos == i = left ++ c:(highlightString' xs ((i, len):ys)) -- start of highlight range
        | pos == i + len - 1 = c:right ++ (highlightString' xs ys) -- end of highlight range
        | otherwise = c:(highlightString' xs ranges) -- inside or outside a highlight range
    highlightString' input [] = map fst input

matchText :: String -> String -> String -> String -> Maybe String
matchText regex text left right = case matches of
    [] -> Nothing
    _ -> Just $ highlightString text left right matches
    where
    matches = getAllMatches ((text =~ regex) :: AllMatches [] (MatchOffset, MatchLength))
