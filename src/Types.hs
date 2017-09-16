module Types where

data Matcher = Lit String -- "abc"                        [x]
             | Many Matcher -- many(...)                  [x]
             | Many1 Matcher -- many1(...)                [x]
             | Matcher `Or` Matcher -- ... Or ...
             | OneOf [Matcher] -- One of: [..., ...]
             | Matcher `And` Matcher -- ... + ...
             | NTimes Int Matcher -- 3times(...)          [x]
             deriving Show
