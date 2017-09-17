module Types where
import Text.Parsec (Parsec)

type Parser a = Parsec String () a

type Definition = (String, Matcher)
                                                       -- Written parser
data Matcher = Lit String -- "abc"                        [x]
             | Many Matcher -- many(...)                  [x]
             | Many1 Matcher -- many1(...)                [x]
             | Matcher `Or` Matcher -- ... Or ...         [x]
             | OneOf [Matcher] -- One of: [..., ...]      [ ]
             | Matcher `And` Matcher -- ... + ...         [x]
             | NTimes Int Matcher -- 3times(...)          [x]
             | Var String -- foobar                       [x]
             | Raw String -- raw regex: /ab+/             [ ]
             deriving Show
