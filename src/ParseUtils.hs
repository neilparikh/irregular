module ParseUtils where
import Text.Parsec (char)
import Types (Parser)

doubleQuotes :: Parser a -> Parser a
doubleQuotes subParser = char '"' *> subParser <* char '"'
