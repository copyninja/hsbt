module Bencode where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Control.Applicative

data BVal =
    Bint Integer
  | Bstr String
  | Blist [BVal]
  | Bdict (M.Map BVal BVal)

ndigit :: Parser Char
ndigit = satisfy (`elem` "123456789")

bencNonZeroInt :: Parser Integer
bencNonZeroInt = do _ <- spaces
                    ds <- between (char 'i') (char 'e') (many1 ndigit)
                    return (read ds)

bencZeroInt :: Parser Integer
bencZeroInt = do _ <- spaces
                 d <- between (char 'i') (char 'e') (char '0')
                 return (read [d])

bencInt :: Parser Integer
bencInt = choice [try bencNonZeroInt, try bencZeroInt]

-- runParser bencInt () "binteger" () "  i3e"

bencStr :: Parser String
bencStr = do _ <- spaces
             ds <- many1 digit <* char ':'
             count (read ds) anyChar
