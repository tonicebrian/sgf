-- boilerplate {{{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Data.SGF.Parse.Raw where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Char
import Data.Tree
import Data.Word
import Prelude hiding (lex)
import Text.Parsec (SourcePos(..), incSourceColumn)
import Text.Parsec.Prim
import Text.Parsec.Combinator
-- }}}
data Property = Property {
    position :: SourcePos,
    name     :: String,
    values   :: [[Word8]]
} deriving (Eq, Ord, Show)

ensure p x = guard (p x) >> return x
enum = toEnum . fromEnum

satisfy p = tokenPrim
    ((\x -> ['\'', x, '\'']) . enum)
    (\pos _ _ -> incSourceColumn pos 1)
    (ensure p)
satisfyChar = satisfy . (. enum)

anyWord     = satisfy (const True)
exactWord   = satisfy . (==) . enum
someWord    = satisfy . flip elem . map enum
noWord      = satisfy . flip notElem . map enum

whitespace  = many (satisfyChar isSpace)

-- assumed: the current byte is literally ASCII '\\' iff the current byte is
-- the last byte of the encoding of the actual character '\\' and neither of
-- the bytes that are literally ASCII ']' and ASCII ':' occur after the first
-- byte of any multi-byte encoded character
-- (in particular, UTF-8, ASCII, and ISO 8859-1 satisfy this property)
escapedChar             = liftM2 (\x y -> [x, y]) (exactWord '\\') anyWord
unescapedExcept      ws = fmap return (noWord ws)
literalTextExcept    ws = fmap concat $ many (escapedChar <|> unescapedExcept ws)

property = liftM3 ((. map enum) . Property)
    (getPosition)
    (many1 (satisfyChar (liftM2 (&&) isUpper (< '\128'))))
    (sepEndBy1 (exactWord '[' >> literalTextExcept "]" <* exactWord ']') whitespace)

node = do
    exactWord ';'
    whitespace
    sepEndBy property whitespace

gameTree = do
    exactWord '('
    whitespace
    (node:nodes) <- sepEndBy1 node     whitespace
    trees        <- sepEndBy  gameTree whitespace
    exactWord ')'
    return (Node node (foldr ((return .) . Node) trees nodes))

collection = whitespace >> sepEndBy1 gameTree whitespace <* whitespace <* eof
