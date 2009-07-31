-- boilerplate {{{
module SGF.Parse.Util where

import Control.Arrow
import Control.Monad.Error hiding (Error(..))
import qualified Control.Monad.Error as Either
import Control.Monad.State hiding (State(..))
import Control.Monad.Writer
import Data.Encoding
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map(..), fromList)
import Data.Ord
import Data.Tree
import Data.Word
import Text.Parsec hiding (State(..))

import SGF.Parse.Raw
import SGF.Types
-- }}}
-- new types: Header, Error, Warning, State, Translator a {{{
data Header = Header {
    format   :: Integer,
    encoding :: DynEncoding
}

data Error
    = UnknownEncoding           { errorPosition :: SourcePos }
    | AmbiguousEncoding         { errorPosition :: SourcePos }
    | FormatUnsupported         { errorPosition :: SourcePos }
    | GameUnsupported           { errorPosition :: SourcePos }
    | ExtraPropertyValues       { errorPosition :: SourcePos }
    | OutOfBounds               { errorPosition :: SourcePos }
    | UnknownError              (Maybe String)
    deriving (Eq, Ord, Show)

instance Either.Error Error where
    noMsg  = UnknownError Nothing
    strMsg = UnknownError . Just

die         :: Error -> Translator a
dieWithJust :: (SourcePos -> Error) -> Maybe Property -> Translator a

die           = lift . StateT . const . Left
dieWithJust e = die . e . position . fromJust

data Warning
    = DuplicateProperty                 Property
    | SquareSizeSpecifiedAsRectangle    SourcePos
    deriving (Eq, Ord, Show)

type State = Tree [Property]
type Translator a = WriterT [Warning] (StateT State (Either Error)) a

transMap :: (a -> Translator b) -> (Maybe a -> Translator (Maybe b))
transMap f = maybe (return Nothing) (liftM Just . f)
-- }}}
-- handy Translators {{{
duplicatesOn :: Ord b => (a -> b) -> [a] -> [a]
duplicatesOn f = map fst
               . concatMap (drop 1)
               . groupBy ((==) `on` snd)
               . sortBy (comparing snd)
               . map (id &&& f)

duplicateProperties :: State -> [Warning]
duplicateProperties = map DuplicateProperty . duplicatesOn name . rootLabel

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

type PTranslator a = Property -> Translator a

number :: PTranslator Integer
number p = return 2 -- TODO

simple :: Header -> PTranslator String
simple header p = return "-32" -- TODO

text :: Header -> PTranslator String
text header p = return "-42" -- TODO

compose :: PTranslator a -> PTranslator b -> PTranslator (a, b)
compose a b p = liftM2 (,) (a p) (b p) -- TODO
-- }}}
