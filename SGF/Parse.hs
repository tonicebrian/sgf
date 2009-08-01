-- boilerplate {{{
{-# LANGUAGE NoMonomorphismRestriction #-}
module SGF.Parse where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.Encoding
import Data.Tree
import Text.Parsec hiding (newline)
import Text.Parsec.Pos (newPos)

import SGF.Parse.Encodings
import SGF.Parse.Raw hiding (gameTree, collection)
import SGF.Parse.Util
import SGF.Types hiding (GeneralHeader(..),Header(..))
import SGF.Types (GeneralHeader(GeneralHeader))
import qualified SGF.Parse.Raw as Raw
import qualified SGF.Types     as T
-- }}}
-- top level/testing {{{
translate :: Monad m => Translator a -> Tree [Property] -> ParsecT s u m (a, [Warning])
translate trans state = case runStateT (runWriterT trans) state of
    Left (UnknownError Nothing ) -> fail ""
    Left (UnknownError (Just e)) -> fail e
    Left e                       -> setPosition (errorPosition e) >> fail (show e)
    Right ((a, warnings), _)     -> return (a, warnings)

collection = second concat . unzip <$> (mapM (translate gameTree) =<< Raw.collection)
gameTree = do
    hea <- parseHeader
    app <- application hea
    gam <- gameType
    var <- variationType
    siz <- size gam
    return (T.Header (GeneralHeader app gam var siz) Nothing)
-- }}}
-- game header information {{{
getFormat   :: Translator Integer
getEncoding :: Translator DynEncoding
parseHeader :: Translator Header

getFormat   = do
    prop <- consumeSingle "FF"
    ff   <- maybe (return 1) number prop
    when (ff /= 4) (dieWithPos FormatUnsupported (maybe (newPos "FF_missing" 1 1) position prop))
    return ff

getEncoding = do
    ws <- consumeSingle "CA"
    case maybe [encodingFromString "latin1"] (guessEncoding . head . values) ws of
        [encoding]  -> return encoding
        []          -> dieWithJust UnknownEncoding   ws
        _           -> dieWithJust AmbiguousEncoding ws -- pretty much guaranteed not to happen

parseHeader = liftM2 Header getFormat getEncoding

application         :: Header -> Translator (Maybe (String, String))
gameType            :: Translator GameType
variationType       :: Translator (Maybe (VariationType, AutoMarkup))
size                :: GameType -> Translator (Maybe (Integer, Integer))
application header  = consumeSingle "AP" >>= transMap (join compose (simple header))
gameType            = do
    property <- consumeSingle "GM"
    gameType <- maybe (return 1) number property
    if enum (minBound :: GameType) <= gameType && gameType <= enum (maxBound :: GameType)
        then return (enum gameType)
        else dieWithJust OutOfBounds property
variationType = consumeSingle "ST" >>= \p -> transMap (number >=> variationType' p) p where
    variationType' property 0 = return (Children, True)
    variationType' property 1 = return (Siblings, True)
    variationType' property 2 = return (Children, False)
    variationType' property 3 = return (Siblings, False)
    variationType' property _ = dieWithJust OutOfBounds property
size gameType = do
    property <- consumeSingle "SZ"
    case property of
        Nothing -> return $ lookup gameType defaultSize
        Just p  -> if enum ':' `elem` head (values p)
            then do
                (m, n) <- join compose number p
                when (m == n) . tell . return . SquareSizeSpecifiedAsRectangle . position $ p
                checkValidity gameType m n property
            else do
                m <- number p
                checkValidity gameType m m property
    where
    invalid       t m n   = or [t == Go && (m > 52 || n > 52), m < 1, n < 1]
    checkValidity t m n p = when (invalid t m n) (dieWithJust OutOfBounds p) >> return (Just (m, n))
-- }}}
-- game-specific stuff {{{
defaultSize = [
    (Go             , (19, 19)),
    (Chess          , ( 8,  8)),
    (LinesOfAction  , ( 8,  8)),
    (Hex            , (11, 11)),
    (Amazons        , (10, 10)),
    (Gess           , (20, 20))
    ]
-- }}}
