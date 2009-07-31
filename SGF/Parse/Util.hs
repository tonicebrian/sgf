-- boilerplate {{{
module SGF.Parse.Util where

import Control.Arrow
import Control.Monad.Error hiding (Error(..))
import qualified Control.Monad.Error as Either
import Control.Monad.State hiding (State(..))
import Control.Monad.Writer
import Data.Char
import Data.Encoding
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map(..), fromList)
import Data.Ord
import Data.Tree
import Data.Word
import Text.Parsec hiding (State(..), newline)

import SGF.Parse.Encodings
import SGF.Parse.Raw
import SGF.Types
-- }}}
-- new types: Header, Error, Warning, State, Translator a, PTranslator a {{{
data Header = Header {
    format   :: Integer,
    encoding :: DynEncoding
}

data ErrorType
    = UnknownEncoding
    | AmbiguousEncoding
    | FormatUnsupported
    | GameUnsupported
    | ExtraPropertyValues
    | OutOfBounds
    | BadlyFormattedValue
    | BadlyEncodedValue
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Error
    = KnownError   { errorType :: ErrorType, errorPosition :: SourcePos }
    | UnknownError { errorDescription :: Maybe String }
    deriving (Eq, Ord, Show)

instance Either.Error Error where
    noMsg  = UnknownError Nothing
    strMsg = UnknownError . Just

die         :: Error -> Translator a
dieWithPos  :: ErrorType -> SourcePos -> Translator a
dieWith     :: ErrorType -> Property -> Translator a
dieWithJust :: ErrorType -> Maybe Property -> Translator a

die           = lift . StateT . const . Left
dieWithPos  e = die . KnownError e
dieWith     e = dieWithPos e . position
dieWithJust e = dieWith e . fromJust

data Warning
    = DuplicatePropertyOmitted          Property
    | SquareSizeSpecifiedAsRectangle    SourcePos
    | DanglingEscapeCharacterOmitted    SourcePos
    deriving (Eq, Ord, Show)

type State = Tree [Property]
type Translator a = WriterT [Warning] (StateT State (Either Error)) a

transMap :: (a -> Translator b) -> (Maybe a -> Translator (Maybe b))
transMap f = maybe (return Nothing) (liftM Just . f)

type PTranslator a = Property -> Translator a
-- }}}
-- handy Translators {{{
duplicatesOn :: Ord b => (a -> b) -> [a] -> [a]
duplicatesOn f = map fst
               . concatMap (drop 1)
               . groupBy ((==) `on` snd)
               . sortBy (comparing snd)
               . map (id &&& f)

duplicateProperties :: State -> [Warning]
duplicateProperties = map DuplicatePropertyOmitted . duplicatesOn name . rootLabel

duplicates :: Translator ()
duplicates = get >>= tell . duplicateProperties

consume :: String -> Translator (Maybe Property)
consume s = do
    (v, rest) <- gets (partition ((== s) . name) . rootLabel)
    modify (\s -> s { rootLabel = rest })
    return (listToMaybe v)

consumeSingle :: String -> Translator (Maybe Property)
consumeSingle s = do
    maybeProperty <- consume s
    case maybeProperty of
        Just (Property { values = (_:_:_) }) -> dieWithJust ExtraPropertyValues maybeProperty
        _ -> return maybeProperty

unknownProperties :: Translator (Map String [[Word8]])
unknownProperties = gets (fromList . map (name &&& values) . rootLabel)

readNumber :: String -> SourcePos -> Translator Integer
readNumber s pos | all isDigit s = return (read s)
                 | otherwise     = dieWithPos BadlyFormattedValue pos

newline :: a -> (String -> a) -> (Char -> String -> a) -> String -> a
newline empty with without xs = case xs of
    '\r':'\n':xs -> with xs
    '\n':'\r':xs -> with xs
    '\r':     xs -> with xs
    '\n':     xs -> with xs
    x   :     xs -> without x xs
    []           -> empty

trim :: Char -> Char
trim x = if isSpace x then ' ' else x

descape :: Char -> SourcePos -> String -> Translator String
descape hard pos s = case s of
    ('\\':xs) -> newline' (tell [DanglingEscapeCharacterOmitted pos]) id xs
    xs        -> newline' (return ()) (hard:) xs
    where
    newline' warn prefix = newline (warn >> return "") (fmap prefix . descape hard pos) (\c -> fmap (trim c:) . descape hard pos)

decodeAndDescape :: Char -> Header -> PTranslator String
decodeAndDescape hard (Header { encoding = e }) (Property { values = v:_, position = pos }) =
    case decodeWordStringExplicit e v of
        Left exception -> dieWithPos BadlyEncodedValue pos
        Right decoded  -> descape hard pos decoded

splitColon  ::  [Word8]  -> Maybe ( [Word8] ,  [Word8] )
splitColons :: [[Word8]] -> Maybe ([[Word8]], [[Word8]])

splitColons = fmap unzip . mapM splitColon
splitColon xs
    | null xs                     = Nothing
    | [enum ':' ] `isPrefixOf` xs = Just ([], drop 1 xs)
    | [enum '\\'] `isPrefixOf` xs = continue 2
    | otherwise                   = continue 1
    where
    continue n = fmap (first (take n xs ++)) (splitColon (drop n xs))

number :: PTranslator Integer
number (Property { values = v:_, position = pos })
    | [enum '+'] `isPrefixOf` v = result 1
    | [enum '-'] `isPrefixOf` v = fmap negate (result 1)
    | otherwise                 = result 0
    where
    result n = readNumber (map enum $ drop n v) pos

simple :: Header -> PTranslator String
text   :: Header -> PTranslator String

simple = decodeAndDescape ' '
text   = decodeAndDescape '\n'

compose :: PTranslator a -> PTranslator b -> PTranslator (a, b)
compose a b p@(Property { values = vs }) = case splitColons vs of
    Nothing       -> dieWith BadlyFormattedValue p
    Just (as, bs) -> liftM2 (,) (a p { values = as }) (b p { values = bs })
-- }}}
