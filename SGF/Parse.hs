{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module SGF.Parse where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Data.Word
import Prelude hiding (lex)
import Text.Parsec (SourcePos(..), incSourceColumn, incSourceLine)
import Text.Parsec.Prim
import Text.Parsec.Combinator

import SGF.Types hiding (Game(..))
import SGF.Types (Game(Game))
import qualified SGF.Types as Game (Game(..))

data ParseHeader = ParseHeader {
    application     :: Maybe ([Word8], [Word8]),
    charset         :: [Word8],
    format          :: Integer,
    gameType        :: Maybe GameType,
    variationType   :: Maybe (VariationType, AutoMarkup),
    size            :: Maybe (Integer, Integer)
    } deriving (Eq, Ord, Show, Read)

emptyHeader = ParseHeader {
    application     = Nothing,
    charset         = map enum "ISO-8859-1",
    format          = 1,
    gameType        = Nothing,
    variationType   = Nothing,
    size            = Nothing
    }

-- parsing the raw structure {{{
ensure p x = guard (p x) >> return x

enum = toEnum . fromEnum
satisfy p = tokenPrim
    ((\x -> ['\'', x, '\'']) . enum)
    (\pos _ _ -> incSourceColumn pos 1)
    (ensure p)
satisfyChar = satisfy . (. enum)

anyWord         = satisfy (const True)
exactWord       = satisfy . (==) . enum
someWord        = satisfy . flip elem . map enum
noWord          = satisfy . flip notElem . map enum

whitespace      = many (satisfyChar isSpace)

-- assumed: the current byte is literally ASCII '\\' iff the current
-- byte is the last byte of the encoding of the actual character '\\'
-- (in particular, UTF-8, ASCII, ISO 8859-1, and EBCDIC satisfy this
-- property)
escapedChar             = liftM2 (\x y -> [x, y]) (exactWord '\\') anyWord
unescapedExcept      ws = fmap return (noWord ws)
literalTextExcept    ws = fmap concat $ many (escapedChar <|> unescapedExcept ws)

propertyRaw = liftM2 (,)
    (many1 (satisfyChar isUpper))
    (sepEndBy1 (exactWord '[' >> literalTextExcept "]" <* exactWord ']') whitespace)

nodeRaw = do
    exactWord ';'
    whitespace
    sepEndBy propertyRaw whitespace

gameTreeRaw = do
    exactWord '('
    whitespace
    (node:nodes) <- sepEndBy1 nodeRaw     whitespace
    trees        <- sepEndBy  gameTreeRaw whitespace
    exactWord ')'
    return (Node node (foldr ((return .) . Node) trees nodes))

collectionRaw = whitespace >> many1 gameTreeRaw <* whitespace <* eof
-- }}}

errorDangling = error "Impossible: dangling escape character"
newline empty withNewline withoutNewline xs = case xs of
    '\r':'\n':xs -> withNewline xs
    '\n':'\r':xs -> withNewline xs
    '\r':     xs -> withNewline xs
    '\n':     xs -> withNewline xs
    x   :     xs -> withoutNewline x xs
    []           -> empty

descapeChar soft x xs      = (if isSpace x then ' ' else x) : descapeText soft xs
descapeText soft ('\\':xs) = newline errorDangling (descapeText soft)           (descapeChar soft) xs
descapeText soft xs        = newline ""            ((soft:) . descapeText soft) (descapeChar soft) xs

descapeSimple = descapeText ' '
descape       = descapeText '\n'

hasDuplicate = any (not . null . drop 1) . group . sort
