-- boilerplate {{{
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Data.SGF.Parse.Raw (
    collection,
    Property(..),
    enum
) where

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
    position :: SourcePos, -- ^
                           -- Currently, this is pretty lame: it doesn't track
                           -- line number and character number, only byte
                           -- offset from the beginning of the file.  This is
                           -- because I don't really understand how to
                           -- correctly track line number and character number
                           -- properly in the face of dynamically changing
                           -- encodings, whereas byte number is a totally
                           -- braindead statistic to track.
    name     :: String,    -- ^
                           -- The literal name of the property.  This is
                           -- guaranteed to be a non-empty string of
                           -- upper-case ASCII characters.
    values   :: [[Word8]]  -- ^ The arguments to the property.
} deriving (Eq, Ord, Show)

-- |
-- Handy way to convert known-ASCII characters from 'Word8' to 'Char', among other
-- things.
enum :: (Enum a, Enum b) => a -> b
enum = toEnum . fromEnum
ensure p x = guard (p x) >> return x

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

-- |
-- Parse the tree-structure of an SGF file, but without any knowledge of the
-- semantics of the properties, etc.
collection :: Stream s m Word8 => ParsecT s u m [Tree [Property]]
collection = whitespace >> sepEndBy1 gameTree whitespace <* whitespace <* eof
