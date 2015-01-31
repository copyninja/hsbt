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
  deriving (Show, Ord, Eq)

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

-- runParser bencVal () "val" "le"
-- runParser bencVal () "val" "l4:spam4:eggse"

bencDict :: Parser (M.Map BVal BVal)
bencDict = between (char 'd') (char 'e') $ M.fromList <$> (many kvpair)
  where kvpair = do k <- bencStr
                    v <- bencVal
                    return (Bstr k, v)

-- runParser bencVal () "val" "d3:cow3:moo4:spam4:eggse"
-- runParser bencVal () "val" "d4:spaml1:a1:bee"
-- runParser bencVal () "val" "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee"
-- runParser bencVal () "val" "de"

bencVal :: Parser BVal
bencVal = Bint <$> bencInt <|>
          Bstr <$> bencStr <|>
          Blist <$> bencList <|>
          Bdict <$> bencDict
