{-# LANGUAGE EmptyDataDecls #-}
module SGF.Types where

import Data.List
import Data.Map hiding (findIndex)
import Data.Maybe
import Data.Tree
import Data.Word

data Void
instance Eq   Void where _ == _ = True
instance Ord  Void where compare _ _ = EQ
instance Read Void where readsPrec _ _ = []
instance Show Void where show _  = ""

data GameType =
    Go | Othello | Chess | Gomoku | NineMen'sMorris |
    Backgammon | ChineseChess | Shogi | LinesOfAction | Ataxx |
    Hex | Jungle | Neutron | Philosopher'sFootball |
    Quadrature | Trax | Tantrix | Amazons | Octi | Gess |
    Twixt | Zertz | Plateau | Yinsh | Punct | Gobblet | Hive |
    Exxit | Hnefatal | Kuba | Tripples | Chase | TumblingDown |
    Sahara | Byte | Focus | Dvonn | Tamsk | Gipf | Kropki
    deriving (Eq, Ord, Bounded, Show, Read)

allGameTypesInSGFOrder =
    [Go, Othello, Chess, Gomoku, NineMen'sMorris, Backgammon,
     ChineseChess, Shogi, LinesOfAction, Ataxx, Hex, Jungle,
     Neutron, Philosopher'sFootball, Quadrature, Trax, Tantrix,
     Amazons, Octi, Gess, Twixt, Zertz, Plateau, Yinsh, Punct,
     Gobblet, Hive, Exxit, Hnefatal, Kuba, Tripples, Chase,
     TumblingDown, Sahara, Byte, Focus, Dvonn, Tamsk, Gipf,
     Kropki
    ]

instance Enum GameType where
    toEnum   n = allGameTypesInSGFOrder !! (n - 1)
    fromEnum t = (+1) . fromJust . findIndex (t==) $ allGameTypesInSGFOrder

type Collection         = [Game]
type Application        = String
type Version            = String
type Point              = (Integer, Integer) -- 0-indexed
type AutoMarkup         = Bool
data FuzzyBool          = Possibly  | Definitely    deriving (Eq, Ord, Show, Read, Enum, Bounded)
data VariationType      = Children  | Siblings      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Emphasis           = Normal    | Strong        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Color              = Black     | White         deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Certainty          = Uncertain | Certain       deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPosition    = Beginning | End           deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RankScale          = Kyu | Dan | Pro           deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Judgment           = GoodForWhite | GoodForBlack | Even | Unclear              deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ViewerSetting      = Tried | Marked | LastMove | Headings | Lock               deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPlacement   = Standard | ScrambledEggs | Parachute | Gemma | Custom     deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RuleSetGo          = AGA | GOE | Chinese | Japanese | NewZealand               deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RuleSetBackgammon  = Crawford | CrawfordGame | Jacoby                          deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RuleSetOcti        = OctiRuleSet MajorVariation [MinorVariation]               deriving (Eq, Ord, Show, Read)
data MajorVariation     = Full | Fast | Kids                                        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MinorVariation     = Edgeless | Superprong | OtherMinorVariation String        deriving (Eq, Ord, Show, Read)
data RuleSet a          = Known !a | OtherRuleSet String                            deriving (Eq, Ord, Show, Read)
data WinType            = Score Rational | Resign | Time | Forfeit | OtherWinType   deriving (Eq, Ord, Show, Read)
data GameResult         = Draw | Void | Unknown | Win Color WinType                 deriving (Eq, Ord, Show, Read)
data Quality            = Bad Emphasis | Doubtful | Interesting | Good Emphasis     deriving (Eq, Ord, Show, Read)
data Rank               = Ranked Integer RankScale (Maybe Certainty) | OtherRank String         deriving (Eq, Ord, Show, Read)

data Round = SimpleRound    Integer
           | FormattedRound Integer String
           | OtherRound             String
    deriving (Eq, Ord, Show, Read)

data MatchInfo = Length           Integer
               | GameNumber       Integer
               | StartScore Color Integer
               | OtherMatchInfo String String
    deriving (Eq, Ord, Show, Read)

data PartialDate
    = Year  { year :: Integer }
    | Month { year :: Integer, month :: Integer }
    | Day   { year :: Integer, month :: Integer, day :: Integer }
    deriving (Eq, Ord, Show, Read)

data Game = Game {
    application     :: Maybe (Application, Version),
    variationType   :: Maybe (VariationType, AutoMarkup),
    size            :: Maybe (Integer, Integer),
    tree            :: GameTree
    } deriving (Eq, Show, Read)

data GameTree
    = TreeGo                          TreeGo
    | TreeBackgammon                  TreeBackgammon
    | TreeLinesOfAction               TreeLinesOfAction
    | TreeHex [(ViewerSetting, Bool)] TreeHex
    | TreeOcti                        TreeOcti
    | TreeOther GameType              TreeOther
    deriving (Eq, Show, Read)

type TreeGo            = Tree NodeGo
type TreeBackgammon    = Tree NodeBackgammon
type TreeLinesOfAction = Tree NodeLinesOfAction
type TreeHex           = Tree NodeHex
type TreeOcti          = Tree NodeOcti
type TreeOther         = Tree NodeOther

data GameNode move stone ruleSet extraGameInfo = GameNode {
    gameInfo    :: Maybe (GameInfo ruleSet extraGameInfo),
    action      :: Either (Setup stone) (Move move),
    annotation  :: Annotation,
    markup      :: Markup,
    unknown     :: Map String [[Word8]]
    } deriving (Eq, Ord, Show, Read)
emptyGameNode = GameNode Nothing (Left emptySetup) emptyAnnotation emptyMarkup empty

type NodeGo            = GameNode MoveGo Point RuleSetGo         GameInfoGo
type NodeBackgammon    = GameNode ()     ()    RuleSetBackgammon GameInfoBackgammon
type NodeLinesOfAction = GameNode ()     ()    Void              GameInfoLinesOfAction
type NodeHex           = GameNode ()     ()    Void              GameInfoHex
type NodeOcti          = GameNode ()     ()    RuleSetOcti       GameInfoOcti
type NodeOther         = GameNode ()     ()    Void              ()

data Move move = Move {
    move                :: Maybe (Color, move),
    illegal             :: FuzzyBool,
    number              :: Maybe Integer,
    quality             :: Maybe Quality,
    timeBlack           :: Maybe Rational,
    timeWhite           :: Maybe Rational,
    overtimeMovesBlack  :: Maybe Integer,
    overtimeMovesWhite  :: Maybe Integer
    } deriving (Eq, Ord, Show, Read)
emptyMove = Move Nothing Possibly Nothing Nothing Nothing Nothing Nothing Nothing

data MoveGo = Pass | Play Point deriving (Eq, Ord, Show, Read)

data Setup stone = Setup {
    addBlack :: [stone],
    addWhite :: [stone],
    remove   :: [Point],
    toPlay   :: Maybe Color
    } deriving (Eq, Ord, Show, Read)
emptySetup = Setup [] [] [] Nothing

data GameInfo ruleSet extra = GameInfo {
    rankBlack       :: Maybe Rank,
    rankWhite       :: Maybe Rank,
    teamNameBlack   :: Maybe String,
    teamNameWhite   :: Maybe String,
    playerNameBlack :: Maybe String,
    playerNameWhite :: Maybe String,
    annotator       :: Maybe String,
    source          :: Maybe String,
    user            :: Maybe String,
    copyright       :: Maybe String,
    date            :: Maybe [PartialDate],
    context         :: Maybe String,
    location        :: Maybe String,
    event           :: Maybe String,
    round           :: Maybe Round,
    game            :: Maybe String,
    opening         :: Maybe String,
    overtime        :: Maybe String,
    ruleSet         :: Maybe (RuleSet ruleSet),
    timeLimit       :: Maybe Rational,
    result          :: Maybe GameResult,
    other           :: extra
    } deriving (Eq, Ord, Show, Read)
emptyGameInfo = GameInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing -- lololololol

data GameInfoGo            = GameInfoGo             { handicap :: Maybe Integer, komi :: Maybe Rational }                                                                           deriving (Eq, Ord, Show, Read)
data GameInfoBackgammon    = GameInfoBackgammon     { match :: Maybe [MatchInfo] }                                                                                                  deriving (Eq, Ord, Show, Read)
data GameInfoLinesOfAction = GameInfoLinesOfAction  { initialPositionLOA :: InitialPosition, invertYAxis :: Bool, initialPlacement :: InitialPlacement }                            deriving (Eq, Ord, Show, Read)
data GameInfoHex           = GameInfoHex            { initialPositionHex :: Maybe () }                                                                                              deriving (Eq, Ord, Show, Read)
data GameInfoOcti          = GameInfoOcti           { squaresWhite :: Maybe [Point], squaresBlack :: Maybe [Point], prongs :: Integer, reserve :: Integer, superProngs :: Integer } deriving (Eq, Ord, Show, Read)

data Annotation = Annotation {
    comment     :: Maybe String,
    name        :: Maybe String,
    hotspot     :: Maybe Emphasis,
    value       :: Maybe Rational,
    judgment    :: Maybe (Judgment, Emphasis)
    } deriving (Eq, Ord, Show, Read)
emptyAnnotation = Annotation Nothing Nothing Nothing Nothing Nothing

data Markup = Markup deriving (Eq, Ord, Show, Read)
emptyMarkup = Markup
