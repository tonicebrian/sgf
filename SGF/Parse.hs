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
import Data.List
import Data.Maybe
import Data.Encoding
import Data.Tree
import Text.Parsec hiding (newline)
import Text.Parsec.Pos (newPos)

import SGF.Parse.Encodings
import SGF.Parse.Raw hiding (gameTree, collection)
import SGF.Types     hiding (GeneralGameInfo(..), GeneralHeader(..), GameInfo(..), Header(..))
import SGF.Parse.Util
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

-- TODO: delete the commented-out test code
--collection = mapM (translate (consume "AAA" >>= transMap real)) =<< Raw.collection
collection = second concat . unzip <$> (mapM (translate gameTree) =<< Raw.collection)
gameTree = do
    hea <- parseHeader
    app <- application hea
    gam <- gameType
    var <- variationType
    siz <- size gam
    inf <- generalGameInfo hea
    return (T.Header (T.GeneralHeader app gam var siz) Nothing, inf)

-- TODO: delete "test" and "stupid"
test = mapM (translate (consume "AAA" >>= transMap (elistOfPoint stupid))) =<< Raw.collection
stupid (Property { values = (a:b:_):_ }) = return (enum a - enum 'a', enum b - enum 'a')
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
    variationType' property 0 = return (T.Children, True )
    variationType' property 1 = return (T.Siblings, True )
    variationType' property 2 = return (T.Children, False)
    variationType' property 3 = return (T.Siblings, False)
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
-- game-info properties {{{
generalGameInfo header =
    foldM (consumeSimpleGameInfo header) emptyGeneralGameInfo simpleGameInfo            >>=
    consumeUpdateGameInfo      rank   (\g v -> g { T.rankBlack = v }) "BR" header       >>=
    consumeUpdateGameInfo      rank   (\g v -> g { T.rankWhite = v }) "WR" header       >>=
    consumeUpdateGameInfoMaybe result (\g v -> g { T.result    = v }) "RE" header       >>=
    timeLimit

simpleGameInfo = [
    ("AN", \i s -> i { T.annotator        = s }),
    ("BT", \i s -> i { T.teamNameBlack    = s }),
    ("CP", \i s -> i { T.copyright        = s }),
    ("EV", \i s -> i { T.event            = s }),
    ("GN", \i s -> i { T.game             = s }),
    ("GC", \i s -> i { T.context          = s }),
    ("ON", \i s -> i { T.opening          = s }),
    ("OT", \i s -> i { T.overtime         = s }),
    ("PB", \i s -> i { T.playerNameBlack  = s }),
    ("PC", \i s -> i { T.location         = s }),
    ("PW", \i s -> i { T.playerNameWhite  = s }),
    ("SO", \i s -> i { T.source           = s }),
    ("US", \i s -> i { T.user             = s }),
    ("WT", \i s -> i { T.teamNameWhite    = s })
    ]

consumeSimpleGameInfo h g (p, u) = consumeUpdateGameInfo id u p h g
consumeUpdateGameInfo            = consumeUpdateGameInfoMaybe . (return .)
consumeUpdateGameInfoMaybe fromString update property header gameInfo = do
    maybeProp   <- consumeSingle property
    maybeString <- transMap (simple header) maybeProp
    case (maybeProp, maybeString >>= fromString) of
        (Nothing, _) -> return gameInfo
        (_, Nothing) -> dieWithJust BadlyFormattedValue maybeProp
        (_, v)       -> return (update gameInfo v)

abbreviateList xs = xs >>= \(n, v) -> [(n, v), (take 1 n, v)]
-- TODO: can we unify this with the other implementation of reading a rational?
readRational s = liftM3 (\s n d -> s * (fromInteger n + d)) maybeSign maybeNum maybeDen where
    (sign, rest)       = span (`elem` "+-") s
    (numerator, rest') = span isDigit rest
    denominator'       = drop 1 rest' ++ "0"
    denominator        = fromInteger (read denominator') / 10 ^ length denominator'

    maybeSign = lookup sign [("", 1), ("+", 1), ("-", -1)]
    maybeNum  = listToMaybe numerator >> return (read numerator)
    maybeDen  = guard (take 1 rest' `isPrefixOf` "." && all isDigit denominator') >> return denominator

rank s = fromMaybe (OtherRank s) maybeRanked where
    (rank, rest)        = span isDigit s
    (scale, certainty)  = span isAlpha rest
    maybeRank           = listToMaybe rank >> return (read rank)
    maybeScale          = lookup (map toLower scale) scales
    maybeCertainty      = lookup certainty certainties
    maybeRanked = liftM3 Ranked maybeRank maybeScale maybeCertainty
    certainties = [("", Nothing), ("?", Just Uncertain), ("*", Just Certain)]
    scales      = abbreviateList [("kyu", Kyu), ("dan", Dan), ("pro", Pro)]

result (c:'+':score) = liftM2 Win maybeColor maybeWinType where
    maybeColor   = lookup (toLower c) [('b', Black), ('w', White)]
    maybeWinType = lookup (map toLower score) winTypes `mplus` fmap Score (readRational score)
    winTypes     = abbreviateList [("", OtherWinType), ("forfeit", Forfeit), ("time", Time), ("resign", Resign)]

result s = lookup (map toLower s) [("0", Draw), ("draw", Draw), ("void", Void), ("?", Unknown)]

timeLimit gameInfo = fmap (\v -> gameInfo { T.timeLimit = v }) (transMap real =<< consumeSingle "TM")
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
