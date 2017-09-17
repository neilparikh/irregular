module ParseUtils where
import Text.Parsec (char, spaces)
import Types (Parser)

wrapWith :: Parser a -> Parser b -> Parser b
wrapWith wrapper subParser = wrapper *> subParser <* wrapper

wrapWithSpaces :: Parser a -> Parser a
wrapWithSpaces = wrapWith spaces

parens :: Parser a -> Parser a
parens subParser = char '(' *> wrapWithSpaces subParser <* char ')'

doubleQuotes :: Parser a -> Parser a
doubleQuotes subParser = char '"' *> subParser <* char '"'
