module Bencode where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*))
import Data.Functor

data BVal =
    Bint Integer
  | Bstr String
  | Blist [BVal]
  | Bdict (M.Map BVal BVal)
  deriving (Show)

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
-- runParser bencInt () "integer" " i0e"
-- runParser bencInt () "integer" " i1e"

bencStr :: Parser String
bencStr = do _ <- spaces
             ds <- many1 digit <* char ':'
             count (read ds) anyChar

-- runParser bencStr () "string" " 4:spam"
-- runParser bencStr () "string" " 4:spam1"
-- runParser bencStr () "string" " 0:spam"

bencList :: Parser [BVal]
bencList = do _ <- spaces
              between (char 'l') (char 'e') (many bencVal)

bencVal :: Parser BVal
bencVal = Bint <$> bencInt <|>
          Bstr <$> bencStr <|>
          Blist <$> bencList
