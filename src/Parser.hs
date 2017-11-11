module Parser where
import Text.Parsec

import Types
import ParseUtils

parseDefinition :: String -> Either ParseError Definition
parseDefinition = applyParser definitionParser

applyParser :: Parser a -> String -> Either ParseError a
applyParser parser = runParser parser () ""

definitionParser :: Parser Definition
definitionParser = (,) <$> (validVarName <* wrapWithSpaces (char '=')) <*> matcherParser

matcherParser :: Parser Matcher
matcherParser = try orParser <|> try andParser <|> nTimesParser False <|> try (many1Parser False) <|> try (manyParser False) <|> litParser <|> varParser

parensMatcherParser :: Parser Matcher
parensMatcherParser = (try $ parens orParser) <|> (try $ parens andParser) <|> nTimesParser True <|> try (many1Parser True) <|> try (manyParser True) <|> litParser <|> varParser

litParser ::  Parser Matcher
litParser = Lit <$> doubleQuotes (many (noneOf "\""))

many1Parser :: Bool -> Parser Matcher
many1Parser withParens = Many1 <$> (string "oneOrMore(" *> subParser <* char ')')
    where
    subParser = if withParens then parensMatcherParser else matcherParser

manyParser :: Bool -> Parser Matcher
manyParser withParens = Many <$> (string "zeroOrMore(" *> subParser <* char ')')
    where
    subParser = if withParens then parensMatcherParser else matcherParser

orParser :: Parser Matcher
orParser = Or <$> (parensMatcherParser <* wrapWithSpaces (string "or")) <*> (try orParser <|> parensMatcherParser)

andParser :: Parser Matcher
andParser = And <$> (parensMatcherParser <* wrapWithSpaces (string "+")) <*> (try andParser <|> parensMatcherParser)

varParser :: Parser Matcher
varParser = Var <$> validVarName

nTimesParser :: Bool -> Parser Matcher
nTimesParser withParens = do
    nStr <- many1 digit
    let n = read nStr
    _ <- string "Times("
    subMatcher <- subParser
    _ <- char ')'
    return $ NTimes n subMatcher
    where
    subParser = if withParens then parensMatcherParser else matcherParser

validVarName :: Parser String
validVarName = many1 (letter)
