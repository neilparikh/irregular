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
matcherParser = try orParser <|> try and5Parser <|> try and4Parser <|> try and3Parser <|> try andParser <|> nTimesParser False <|> try (many1Parser False) <|> try (manyParser False) <|> litParser <|> varParser

parensMatcherParser :: Parser Matcher
parensMatcherParser = (try $ parens orParser) <|> (try $ parens and5Parser) <|> (try $ parens and4Parser) <|> (try $ parens and3Parser) <|> (try $ parens andParser) <|> nTimesParser True <|> try (many1Parser True) <|> try (manyParser True) <|> litParser <|> varParser

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

and3Parser :: Parser Matcher
and3Parser = do
    first <- parensMatcherParser
    _ <- string " + "
    rest <- andParser
    return $ And first rest

and4Parser :: Parser Matcher
and4Parser = do
    first <- parensMatcherParser
    _ <- string " + "
    rest <- and3Parser
    return $ And first rest

and5Parser :: Parser Matcher
and5Parser = do
    first <- parensMatcherParser
    _ <- string " + "
    rest <- and4Parser
    return $ And first rest

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
