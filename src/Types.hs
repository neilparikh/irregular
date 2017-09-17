module Types where
import Text.Parsec (Parsec)

type Parser a = Parsec String () a

data Matcher = Lit String -- "abc"                        [x]
             | Many Matcher -- many(...)                  [x]
             | Many1 Matcher -- many1(...)                [x]
             | Matcher `Or` Matcher -- ... Or ...
             | OneOf [Matcher] -- One of: [..., ...]
             | Matcher `And` Matcher -- ... + ...
             | NTimes Int Matcher -- 3times(...)          [x]
             deriving Show
