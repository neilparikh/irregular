module Parser where
import Text.Parsec

import Types
import ParseUtils

parseDefinition :: String -> Either ParseError Definition
parseDefinition = applyParser definitionParser
    where
    applyParser :: Parser a -> String -> Either ParseError a
    applyParser parser = runParser parser () ""

definitionParser :: Parser Definition
definitionParser = (,) <$> ((many1 (noneOf "= ")) <* wrapWithSpaces (char '=')) <*> matcherParser

matcherParser :: Parser Matcher
matcherParser = try orParser <|> try andParser <|> nTimesParser False <|> many1Parser False <|> manyParser False <|> litParser

parensMatcherParser :: Parser Matcher
parensMatcherParser = (try $ parens orParser) <|> (try $ parens andParser) <|> nTimesParser True <|> many1Parser True <|> manyParser True <|> litParser

litParser ::  Parser Matcher
litParser = Lit <$> doubleQuotes (many (noneOf "\""))

many1Parser :: Bool -> Parser Matcher
many1Parser withParens = Many1 <$> (string "atLeastOneOf(" *> subParser <* char ')')
    where
    subParser = if withParens then parensMatcherParser else matcherParser

manyParser :: Bool -> Parser Matcher
manyParser withParens = Many <$> (string "many(" *> subParser <* char ')')
    where
    subParser = if withParens then parensMatcherParser else matcherParser

orParser :: Parser Matcher
orParser = Or <$> (parensMatcherParser <* string " or ") <*> parensMatcherParser

andParser :: Parser Matcher
andParser = And <$> (parensMatcherParser <* string " + ") <*> parensMatcherParser

nTimesParser :: Bool -> Parser Matcher
nTimesParser withParens = do
    nStr <- many1  digit
    let n = read nStr
    _ <- string "Times("
    subMatcher <- subParser
    _ <- char ')'
    return $ NTimes n subMatcher
    where
    subParser = if withParens then parensMatcherParser else matcherParser
