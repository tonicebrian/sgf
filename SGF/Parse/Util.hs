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
import Data.Ix
import Data.List
import Data.Maybe
import Data.Map (Map(..), fromList, keys)
import Data.Ord
import Data.Tree
import Data.Word
import Text.Parsec hiding (State(..), choice, newline)

import SGF.Parse.Encodings
import SGF.Parse.Raw
import SGF.Types hiding (name)
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
    | OutOfBounds
    | BadlyFormattedValue
    | BadlyEncodedValue
    | ConcurrentMoveAndSetup
    | ConcurrentBlackAndWhiteMove
    | ConcurrentAnnotations
    | ExtraMoveAnnotations
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

-- by convention, a warning that does not end in a verb just "did the right thing" to correct the problem
data Warning
    = DuplicatePropertyOmitted          Property
    | SquareSizeSpecifiedAsRectangle    SourcePos
    | DanglingEscapeCharacterOmitted    SourcePos
    | PropValueForNonePropertyOmitted   Property
    | UnknownPropertyPreserved          String
    | PointSpecifiedAsPointRange        Property
    | DuplicatePointsOmitted            Property [Point]
    | InvalidDatesClipped               [PartialDate]
    | AnnotationWithNoMoveOmitted       Property
    | ExtraGameInfoOmitted              Property
    | NestedRootPropertyOmitted         Property
    | MovelessAnnotationOmitted         Property
    | DuplicateSetupOperationsOmitted   [Point]
    | ExtraPositionalJudgmentOmitted    (Judgment, Emphasis)
    | DuplicateMarkupOmitted            (Mark, Point)
    | ExtraPropertyValuesOmitted        Property
    deriving (Eq, Ord, Show)

type State = Tree [Property]
type  Translator a = WriterT [Warning] (StateT State (Either Error)) a
type PTranslator a = Property -> Translator a

transMap, transMapMulti :: PTranslator a -> String -> Translator (Maybe a)
transMap      f = consumeSingle >=> transMap' f
transMapMulti f = consume       >=> transMap' f

transMap' :: (a -> Translator b) -> (Maybe a -> Translator (Maybe b))
transMap' f = maybe (return Nothing) (liftM Just . f)

transMapList :: PTranslator [b] -> String -> Translator [b]
transMapList f = consume >=> maybe (return []) f
-- }}}
-- handy Translators {{{
-- helper functions {{{
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

readNumber :: String -> SourcePos -> Translator Integer
readNumber "" _ = return 0
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

warnAboutDuplicatePoints :: Property -> [Point] -> Translator [Point]
warnAboutDuplicatePoints p ps = let ds = duplicatesOn id ps in do
    when (not $ null ds) (tell [DuplicatePointsOmitted p ds])
    return (nub ps)

checkPointList :: (PTranslator [Point] -> PTranslator [[Point]]) -> (PTranslator Point -> PTranslator [Point])
checkPointList listType a p = listType (mayBeCompoundPoint a) p >>= warnAboutDuplicatePoints p . concat
-- }}}
-- low-level {{{
has :: String -> Translator Bool
has s = gets (any ((s ==) . name) . rootLabel)

hasAny :: [String] -> Translator Bool
hasAny = fmap or . mapM has

consume :: String -> Translator (Maybe Property)
consume s = do
    (v, rest) <- gets (partition ((== s) . name) . rootLabel)
    modify (\s -> s { rootLabel = rest })
    return (listToMaybe v)

consumeSingle :: String -> Translator (Maybe Property)
consumeSingle s = do
    maybeProperty <- consume s
    case maybeProperty of
        Just p@(Property { values = (v:_:_) }) -> do
            tell [ExtraPropertyValuesOmitted p]
            return (Just p { values = [v] })
        _ -> return maybeProperty

unknownProperties :: Translator (Map String [[Word8]])
unknownProperties = do
    m <- gets (fromList . map (name &&& values) . rootLabel)
    tell [UnknownPropertyPreserved name | name <- keys m]
    return m
-- }}}
-- PTranslators and combinators {{{
number :: PTranslator Integer
number p@(Property { values = v:_ })
    | enum '.' `elem` v = dieWith BadlyFormattedValue p
    | otherwise         = fmap floor (real p)

real :: PTranslator Rational
real (Property { values = v:_, position = pos })
    | [enum '+'] `isPrefixOf` v = result 1
    | [enum '-'] `isPrefixOf` v = fmap negate (result 1)
    | otherwise                 = result 0
    where
    split  i = second (drop 1) . break (== '.') . map enum . drop i $ v
    result i = let (n, d) = split i in do
        whole <- readNumber n pos
        fract <- readNumber d pos
        return (fromInteger whole + fromInteger fract / 10 ^ length d)

simple :: Header -> PTranslator String
text   :: Header -> PTranslator String

simple = decodeAndDescape ' '
text   = decodeAndDescape '\n'

none :: PTranslator ()
none (Property { values = [[]] }) = return ()
none p = tell [PropValueForNonePropertyOmitted p]

choice :: [([Word8], a)] -> PTranslator a
choice vs p@(Property { values = []  }) = dieWith BadlyFormattedValue p -- can't happen
choice vs p@(Property { values = v:_ }) = maybe (dieWith BadlyFormattedValue p) return (lookup v vs)

choice' :: [(String, a)] -> PTranslator a
choice' vs = choice [(map enum k, v) | (k', v) <- vs, k <- [k', map toLower k']]

double :: PTranslator Emphasis
color  :: PTranslator Color
double = choice' [("1", Normal), ("2", Strong)]
color  = choice' [("B", Black), ("W", White)]

compose :: PTranslator a -> PTranslator b -> PTranslator (a, b)
compose a b p@(Property { values = vs }) = case splitColons vs of
    Nothing       -> dieWith BadlyFormattedValue p
    Just (as, bs) -> liftM2 (,) (a p { values = as }) (b p { values = bs })

listOf :: PTranslator a -> PTranslator [a]
listOf a p@(Property { values = vs }) = mapM a [p { values = [v] } | v <- vs]

elistOf :: PTranslator a -> PTranslator [a]
elistOf _ (Property { values = [[]] }) = return []
elistOf a p = listOf a p

mayBeCompoundPoint, listOfPoint, elistOfPoint :: PTranslator Point -> PTranslator [Point]
mayBeCompoundPoint a p@(Property { values = v:_ }) = case splitColon v of
    Nothing -> fmap return $ a p
    Just {} -> do
        pointRange <- compose a a p { values = [v] }
        when (uncurry (==) pointRange) (tell [PointSpecifiedAsPointRange p])
        return (range pointRange)

listOfPoint  = checkPointList listOf
elistOfPoint = checkPointList elistOf
-- }}}
-- }}}
