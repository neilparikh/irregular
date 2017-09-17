module Parser where
import Text.Parsec

import Types
import ParseUtils

parseMatcher :: String -> Either ParseError Matcher
parseMatcher = applyParser matcherParser
    where
    applyParser :: Parser a -> String -> Either ParseError a
    applyParser parser = runParser parser () ""

matcherParser :: Parser Matcher
matcherParser = nTimesParser <|> many1Parser <|> manyParser <|> litParser

litParser :: Parser Matcher
litParser = Lit <$> doubleQuotes (many (noneOf "\""))

many1Parser :: Parser Matcher
many1Parser = Many1 <$> (string "atLeastOneOf(" *> matcherParser <* char ')')

manyParser :: Parser Matcher
manyParser = Many <$> (string "many(" *> matcherParser <* char ')')

nTimesParser :: Parser Matcher
nTimesParser = do
    nStr <- many1  digit
    let n = read nStr
    _ <- string "Times("
    subMatcher <- matcherParser
    _ <- char ')'
    return $ NTimes n subMatcher
