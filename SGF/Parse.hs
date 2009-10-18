-- TODO: check that every occurrence of "die" should really be a death, and not just a "fix-it-up-and-warn"
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
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Data.Tree
import Prelude     hiding (round)
import Text.Parsec hiding (newline)
import Text.Parsec.Pos    (newPos)

import qualified Data.Map as Map
import qualified Data.Set as Set

import SGF.Parse.Encodings
import SGF.Parse.Raw hiding (gameTree, collection)
import SGF.Types     hiding (Game(..), GameInfo(..), GameNode(..), Setup(..), Move(..))
import SGF.Types     (Game(Game), GameNode(GameNode))
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

-- TODO: delete "test"
test = runParser collection () "<interactive>" . map enum
collection = second concat . unzip <$> (mapM (translate gameTree) =<< Raw.collection)
gameTree = do
    hea <- parseHeader
    app <- application hea
    gam <- gameType
    var <- variationType
    siz <- size gam
    fmap (Game app var siz) (parse hea gam siz False)
    where
    parse h g s = case g of
        Go            -> fmap TreeGo            . nodeGo h s
        Backgammon    -> fmap TreeBackgammon    . nodeBackgammon h
        LinesOfAction -> fmap TreeLinesOfAction . nodeLinesOfAction h
        Hex           -> gameHex h
        Octi          -> fmap TreeOcti          . nodeOcti h
        other         -> fmap (TreeOther other) . nodeOther h
-- }}}
warnAll     w ps = mapM_ (\p -> maybe (return ()) (tell . (:[]) . w) =<< consume p) ps
dieEarliest e ps = dieWith e . head . sortBy (comparing position) . catMaybes =<< mapM consume ps
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
application = flip transMap "AP" . join compose . simple
gameType    = do
    property <- consumeSingle "GM"
    gameType <- maybe (return 1) number property
    if enum (minBound :: GameType) <= gameType && gameType <= enum (maxBound :: GameType)
        then return (enum gameType)
        else dieWithJust OutOfBounds property
variationType = transMap (\p -> number p >>= variationType' p) "ST" where
    variationType' property 0 = return (T.Children, True )
    variationType' property 1 = return (T.Siblings, True )
    variationType' property 2 = return (T.Children, False)
    variationType' property 3 = return (T.Siblings, False)
    variationType' property _ = dieWith OutOfBounds property
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
gameInfo header =
        foldM (consumeSimpleGameInfo header) (emptyGameInfo ()) simpleGameInfo
    >>= consumeUpdateGameInfo      rank   (\g v -> g { T.rankBlack = v }) "BR" header
    >>= consumeUpdateGameInfo      rank   (\g v -> g { T.rankWhite = v }) "WR" header
    >>= consumeUpdateGameInfo      round  (\g v -> g { T.round     = v }) "RO" header
    >>= consumeUpdateGameInfoMaybe result (\g v -> g { T.result    = v }) "RE" header
    >>= consumeUpdateGameInfoMaybe date   (\g v -> g { T.date      = v }) "DT" header
    >>= warnClipDate
    >>= timeLimit

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
    maybeString <- transMap' (simple header) maybeProp
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

timeLimit gameInfo = fmap (\v -> gameInfo { T.timeLimit = v }) (transMap real "TM")

date = expect [] . splitWhen (== ',') where
    expect parsers [] = return []
    expect parsers (pd:pds) = do
        parsed <- msum . sequence ([parseYMD, parseYM, parseY] ++ parsers) . splitWhen (== '-') $ pd
        liftM (parsed:) . ($ pds) $ case parsed of
            Year  {}                        -> expect []
            Month { year = y }              -> expect [parseMD y, parseM y]
            Day   { year = y, month = m }   -> expect [parseMD y, parseD y m]

    ensure p x = guard (p x) >> return x
    hasLength n xs = n >= 0 && hasLength' n xs where
        hasLength' n []     = n == 0
        hasLength' 0 (x:xs) = False
        hasLength' n (x:xs) = hasLength' (n-1) xs
    ensureLength = ensure . hasLength

    parseYMD    ss = ensureLength 3 ss >>= \[y, m, d] -> liftM3 Day   (checkY y) (checkMD m) (checkMD d)
    parseYM     ss = ensureLength 2 ss >>= \[y, m   ] -> liftM2 Month (checkY y) (checkMD m)
    parseY      ss = ensureLength 1 ss >>= \[y      ] -> liftM  Year  (checkY y)
    parseMD y   ss = ensureLength 2 ss >>= \[   m, d] -> liftM2 (Day y)          (checkMD m) (checkMD d)
    parseM  y   ss = ensureLength 1 ss >>= \[   m   ] -> liftM  (Month y)        (checkMD m)
    parseD  y m ss = ensureLength 1 ss >>= \[      d] -> liftM  (Day y m)                    (checkMD d)

    checkY  y  = ensureLength 4 y  >>= readM
    checkMD md = ensureLength 2 md >>= readM
    readM      = listToMaybe . map fst . filter (null . snd) . reads

clipDate (y@Year  {}) = Year . min 9999 . max 0 . year $ y
clipDate (Month { year = y, month = m }) = Month {
    year  = year . clipDate . Year $ y,
    month = min 12 . max 1 $ m
    }
clipDate (Day { year = y, month = m, day = d }) = let m' = clipDate (Month y m) in Day {
    year = year m', month = month m',
    day  = max 1 . min (fromIntegral (gregorianMonthLength (year m') (fromIntegral (month m')))) $ d
    }

warnClipDate gameInfo@(T.GameInfo { T.date = d }) = let d' = fmap (map clipDate) d in do
    when (d /= d') (tell [InvalidDatesClipped (fromJust d)])
    return gameInfo { T.date = d' }

round s = case words s of
    [roundNumber@(_:_)]                | all isDigit roundNumber
        -> SimpleRound (read roundNumber)
    [roundNumber@(_:_), '(':roundType] | all isDigit roundNumber && last roundType == ')'
        -> FormattedRound (read roundNumber) (init roundType)
    _   -> OtherRound s
-- }}}
-- move properties {{{
move move = do
    color_                                              <- mapM has ["B", "W"]
    [number_, overtimeMovesBlack_, overtimeMovesWhite_] <- mapM (transMap number) ["MN", "OB", "OW"]
    [timeBlack_, timeWhite_]                            <- mapM (transMap real  ) ["BL", "WL"]
    let partialMove = emptyMove {
            T.number                = number_,
            T.timeBlack             = timeBlack_,
            T.timeWhite             = timeWhite_,
            T.overtimeMovesBlack    = overtimeMovesBlack_,
            T.overtimeMovesWhite    = overtimeMovesWhite_
            }
    case color_ of
        [False, False] -> warnAll MovelessAnnotationOmitted ["KO", "BM", "DO", "IT", "TE"] >> return partialMove
        [True , True ] -> dieEarliest ConcurrentBlackAndWhiteMove ["B", "W"]
        [black, white] -> let color = if black then Black else White in do
            Just move   <- fmap msum . mapM (transMap move) $ ["B", "W"]
            illegal     <- fmap (maybe Possibly (const Definitely)) (transMap none "KO")
            annotations <- mapM has ["BM", "DO", "IT", "TE"]
            quality     <- case annotations of
                [False, False, False, False] -> return Nothing
                [True , False, False, False] -> fmap (fmap Bad ) (transMap double "BM")
                [False, False, False, True ] -> fmap (fmap Good) (transMap double "TE")
                [False, True , False, False] -> transMap none "DO" >> return (Just Doubtful   )
                [False, False, True , False] -> transMap none "IT" >> return (Just Interesting)
                _                            -> dieEarliest ConcurrentAnnotations ["BM", "DO", "IT", "TE"]
            return partialMove { T.move = Just (color, move), T.illegal = illegal, T.quality = quality }
-- }}}
-- setup properties {{{
setupPoint point = do
    points <- mapM (transMapList (listOfPoint point)) ["AB", "AW", "AE"]
    let [addBlack, addWhite, remove] = map Set.fromList points
        allPoints  = addBlack `Set.union` addWhite `Set.union` remove
        duplicates = concat points \\ Set.elems allPoints
        addWhite'  = addWhite  Set.\\ addBlack
        remove'    = remove    Set.\\ (addBlack `Set.union` addWhite')
    unless (null duplicates) (tell [DuplicateSetupOperationsOmitted duplicates])
    setupFinish addBlack addWhite' remove'
    where
    points = transMapList (listOfPoint point)

-- note: does not (cannot, in general) check the constraint that addBlack,
-- addWhite, and remove specify disjoint sets of points
-- TODO: what, really, cannot?  even if we allow ourselves a class constraint or something?
setupPointStone point stone = do
    addBlack <- transMapList (listOf      stone) "AB"
    addWhite <- transMapList (listOf      stone) "AW"
    remove   <- transMapList (listOfPoint point) "AE"
    setupFinish (Set.fromList addBlack) (Set.fromList addWhite) (Set.fromList remove)

setupFinish addBlack addWhite remove =
    liftM (T.Setup addBlack addWhite remove) (transMap color "PL")
-- }}}
-- none properties {{{
annotation header = do
    comment     <- transMap (text   header) "C"
    name        <- transMap (simple header) "N"
    hotspot     <- transMap double          "HO"
    value       <- transMap real            "V"
    judgments'  <- mapM (transMap double) ["GW", "GB", "DM", "UC"]
    let judgments = [(j, e) | (j, Just e) <- zip [GoodForWhite ..] judgments']
    tell . map ExtraPositionalJudgmentOmitted . drop 1 $ judgments
    return Annotation { T.comment = comment, T.name = name, T.hotspot = hotspot, T.value = value, T.judgment = listToMaybe judgments }

addMarks marks (mark, points) = tell warning >> return result where
    (ignored, inserted) = partition (`Map.member` marks) points
    warning = map (DuplicateMarkupOmitted . (,) mark) ignored
    result  = marks `Map.union` Map.fromList [(i, mark) | i <- inserted]

markup point = do
    markedPoints <- mapM (transMapList (listOfPoint point)) ["CR", "MA", "SL", "SQ", "TR"]
    marks <- foldM addMarks Map.empty . zip [Circle ..] $ markedPoints
    return Markup { T.marks = marks }
-- }}}
-- known properties list {{{
data PropertyType = Move | Setup | Root | GameInfo | Inherit | None deriving (Eq, Ord, Show, Read, Enum, Bounded)

properties = liftM2 (++) properties' . extraProperties where
    properties' Move     = ["B", "KO", "MN", "W", "BM", "DO", "IT", "TE", "BL", "OB", "OW", "WL"]
    properties' Setup    = ["AB", "AE", "AW", "PL"]
    properties' Root     = ["AP", "CA", "FF", "GM", "ST", "SZ"]
    properties' GameInfo = ["AN", "BR", "BT", "CP", "DT", "EV", "GN", "GC", "ON", "OT", "PB", "PC", "PW", "RE", "RO", "RU", "SO", "TM", "US", "WR", "WT"]
    properties' Inherit  = ["DD", "PM", "VW"]
    properties' None     = ["C", "DM", "GB", "GW", "HO", "N", "UC", "V", "AR", "CR", "LB", "LN", "MA", "SL", "SQ", "TR", "FG"]
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

ruleSetLookup rs  = flip lookup rs . map toLower
ruleSetGo         = ruleSetLookup [
    ("aga"                      , AGA),
    ("goe"                      , GOE),
    ("chinese"                  , Chinese),
    ("japanese"                 , Japanese),
    ("nz"                       , NewZealand)
    ]
ruleSetBackgammon = ruleSetLookup [
    ("crawford"                 , Crawford),
    ("crawford:crawfordgame"    , CrawfordGame),
    ("jacoby"                   , Jacoby)
    ]
ruleSetOcti s = case break (== ':') s of
    (major, ':':minors) -> liftM  (flip OctiRuleSet (minorVariations minors       )) (majorVariation major        )
    (majorOrMinors, "") -> liftM  (flip OctiRuleSet []                             ) (majorVariation majorOrMinors)
                   `mplus` return (OctiRuleSet Full (minorVariations majorOrMinors))
    where
    majorVariation      = ruleSetLookup [("full", Full), ("fast", Fast), ("kids", Kids)]
    minorVariation    s = fromMaybe (OtherMinorVariation s) . ruleSetLookup [("edgeless", Edgeless), ("superprong", Superprong)] $ s
    minorVariations     = map minorVariation . splitWhen (== ',')

ruleSet read maybeDefault header = do
    maybeRulesetString <- transMap (simple header) "RU"
    return $ case (maybeRulesetString, maybeRulesetString >>= read) of
        (Nothing, _      ) -> fmap Known maybeDefault
        (Just s , Nothing) -> Just (OtherRuleSet s)
        (_      , Just rs) -> Just (Known rs)

ruleSetDefault = ruleSet (const Nothing) Nothing

extraProperties Go            GameInfo = ["HA", "KM"]
extraProperties Go            None     = ["TB", "TW"]
extraProperties Backgammon    Setup    = ["CO", "CV", "DI"]
extraProperties Backgammon    GameInfo = ["MI", "RE", "RU"]
extraProperties LinesOfAction GameInfo = ["IP", "IY", "SU"]
extraProperties LinesOfAction None     = ["AS", "SE"]
extraProperties Hex           Root     = ["IS"]
extraProperties Hex           GameInfo = ["IP"]
extraProperties Amazons       Setup    = ["AA"]
extraProperties Octi          Setup    = ["RP"]
extraProperties Octi          GameInfo = ["BO", "WO", "NP", "NR", "NS"]
extraProperties Octi          None     = ["AS", "CS", "MS", "SS", "TS"]
extraProperties _             _        = []

gameInfoGo = liftM2 GameInfoGo (transMap number "HA") (transMap real "KM")

pointGo (Property { values = [[x, y]] }) | valid x && valid y = return (translate x, translate y)
    where
    valid x     = (enum 'a' <= x && x <= enum 'z') || (enum 'A' <= x && x <= enum 'Z')
    translate x = enum x - enum (if x < enum 'a' then 'A' else 'a') + 1
pointGo p = dieWith BadlyFormattedValue p

moveGo _             (Property { values = [[]] }) = return Pass
moveGo (Just (w, h)) p                            = pointGo p >>= \v@(x, y) -> return $ if x > w || y > h then Pass else Play v
moveGo _             p                            = fmap Play (pointGo p)

gameHex header seenGameInfo = fmap (TreeHex []) (nodeHex header seenGameInfo)

nodeGo header size seenGameInfo = do
    [hasGameInfo, hasRoot, hasSetup, hasMove] <- mapM (hasAny . properties Go) [GameInfo, Root, Setup, Move]
    let setGameInfo       = hasGameInfo && not seenGameInfo
        duplicateGameInfo = hasGameInfo && seenGameInfo
    when (hasSetup && hasMove) dieSetupAndMove
    when duplicateGameInfo warnGameInfo
    when hasRoot           warnRoot

    mGameInfo     <- liftM (\x -> guard setGameInfo >> Just x) (gameInfo header)
    extraGameInfo <- gameInfoGo
    ruleSet_      <- ruleSet ruleSetGo Nothing header
    action_       <- if hasMove then liftM Right $ move (moveGo size) else liftM Left $ setupPoint pointGo
    annotation_   <- annotation header
    markup_       <- markup pointGo
    unknown_      <- unknownProperties
    children      <- gets subForest >>= mapM (\s -> put s >> nodeGo header size (seenGameInfo || hasGameInfo))

    return (Node (GameNode (fmap (\gi -> gi { T.ruleSet = ruleSet_, T.other = extraGameInfo }) mGameInfo) action_ annotation_ markup_ unknown_) children)
    where
    dieSetupAndMove    = dieEarliest ConcurrentMoveAndSetup    (properties Go =<< [Setup, Move])
    warnGameInfo       = warnAll     ExtraGameInfoOmitted      (properties Go GameInfo)
    warnRoot           = warnAll     NestedRootPropertyOmitted (properties Go Root)

nodeBackgammon    = nodeOther
nodeLinesOfAction = nodeOther
nodeHex           = nodeOther
nodeOcti          = nodeOther
nodeOther header seenGameInfo = return (Node emptyGameNode [])
-- }}}
