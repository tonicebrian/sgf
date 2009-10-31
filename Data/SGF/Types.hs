-- boilerplate {{{
{-# LANGUAGE EmptyDataDecls #-}
module Data.SGF.Types (
    -- * Game type
    Game(..), GameTree(..), GameNode(..),
    Move(..), Setup(..),
    Annotation(..), Markup(..), GameInfo(..), GameInfoType(..),
    emptyGameNode, emptyMove, emptySetup, emptyGameInfo, emptyAnnotation, emptyMarkup,

    -- * Game-specific types
    -- ** Go
    NodeGo, MoveGo(..), RuleSetGo(..), GameInfoGo(..), AnnotationGo,

    -- ** Backgammon
    NodeBackgammon, RuleSetBackgammon(..), GameInfoBackgammon(..),
    MatchInfo(..),

    -- ** Lines of Action
    NodeLinesOfAction, GameInfoLinesOfAction(..),
    InitialPosition(..), InitialPlacement(..),

    -- ** Hex
    NodeHex, GameInfoHex(..),
    ViewerSetting(..),

    -- ** Octi
    NodeOcti, RuleSetOcti(..), GameInfoOcti(..),
    MajorVariation(..), MinorVariation(..),

    -- ** Other
    NodeOther,

    -- * Type aliases
    Collection, Point,
    Application, Version, AutoMarkup,
    TreeGo, TreeBackgammon, TreeLinesOfAction, TreeHex, TreeOcti, TreeOther,

    -- * Enumerations
    Color(..), RankScale(..),
    Emphasis(..), Certainty(..), FuzzyBool(..),
    GameType(..),
    Judgment(..), Quality(..),
    Mark(..), Numbering(..), VariationType(..), FigureFlag(..),

    -- * Miscellaneous
    WinType(..), GameResult(..), Rank(..), RuleSet(..),
    Round(..), PartialDate(..), Figure(..),
    Void
) where

import Data.List
import Data.Map hiding (empty, filter, findIndex)
import Data.Maybe
import Data.Ord
import Data.Set hiding (empty, filter)
import Data.Tree
import Data.Word

import qualified Data.Map as Map
import qualified Data.Set as Set

data Void
instance Eq   Void where _ == _ = True
instance Ord  Void where compare _ _ = EQ
instance Read Void where readsPrec _ _ = []
instance Show Void where show _  = ""
-- }}}
-- GameType {{{
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
-- }}}
-- type aliases {{{
type Collection         = [Game]
type Application        = String
type Version            = String
-- | 0-indexed x/y coordinates that start at the top left
type Point              = (Integer, Integer)
type AutoMarkup         = Bool
-- }}}
-- enums {{{
data FuzzyBool          = Possibly  | Definitely    deriving (Eq, Ord, Show, Read, Enum, Bounded)
data VariationType      = Children  | Siblings      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Emphasis           = Normal    | Strong        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Color              = Black     | White         deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Certainty          = Uncertain | Certain       deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPosition    = Beginning | End           deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RankScale          = Kyu | Dan | Pro           deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Judgment           = GoodForWhite | GoodForBlack | Even | Unclear              deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Mark               = Circle | X | Selected | Square | Triangle                 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Numbering          = Unnumbered | Numbered | Modulo100                         deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ViewerSetting      = Tried | Marked | LastMove | Headings | Lock               deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPlacement   = Standard | ScrambledEggs | Parachute | Gemma | Custom     deriving (Eq, Ord, Show, Read, Enum, Bounded)
data GameInfoType       = TeamName Color | PlayerName Color | Annotator | Source | User | Copyright | Context | Location | Event | GameName | Opening | Overtime
     deriving (Eq, Ord, Show, Read)

allGameInfoTypes = [TeamName Black, TeamName White, PlayerName Black, PlayerName White, Annotator, Source, User, Copyright, Context, Location, Event, GameName, Opening, Overtime]
instance Enum GameInfoType where
    toEnum = (allGameInfoTypes !!)
    fromEnum t = fromJust $ findIndex (t==) allGameInfoTypes

instance Bounded GameInfoType where
    minBound = head allGameInfoTypes
    maxBound = last allGameInfoTypes
-- }}}
-- rulesets {{{
data RuleSetGo          = AGA | GOE | Chinese | Japanese | NewZealand               deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RuleSetBackgammon  = Crawford | CrawfordGame | Jacoby                          deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RuleSetOcti        = OctiRuleSet MajorVariation [MinorVariation]               deriving (Eq, Ord, Show, Read)
data MajorVariation     = Full | Fast | Kids                                        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MinorVariation     = Edgeless | Superprong | OtherMinorVariation String        deriving (Eq, Ord, Show, Read)
data RuleSet a          = Known !a | OtherRuleSet String                            deriving (Eq, Ord, Show, Read)
-- }}}
-- misc types {{{
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
-- }}}
-- Figure {{{
-- FigureFlag {{{
data FigureFlag         = Coordinates | Name | HiddenMoves | RemoveCaptures | Hoshi deriving (Eq, Ord, Show, Read,       Bounded)

allFigureFlags :: [FigureFlag]
allFigureFlags = [Coordinates, Name, HiddenMoves, RemoveCaptures, Hoshi]

instance Enum FigureFlag where
    toEnum 0 = Coordinates
    toEnum 1 = Name
    toEnum 2 = HiddenMoves
    toEnum 8 = RemoveCaptures
    toEnum 9 = Hoshi
    toEnum n = error $ "unknown FigureFlag bit " ++ show n
    fromEnum Coordinates    = 0
    fromEnum Name           = 1
    fromEnum HiddenMoves    = 2
    fromEnum RemoveCaptures = 8
    fromEnum Hoshi          = 9
    enumFrom   lo    = dropWhile (/= lo) allFigureFlags
    enumFromTo lo hi = filter (\x -> lo <= x && x <= hi) allFigureFlags
    succ       lo    = enumFrom lo !! 1
    pred       hi    = reverse (enumFromTo minBound hi) !! 1
    -- TODO: enumFromThen, enumFromThenTo
-- }}}
data Figure
    = DefaultFigure
    | NamedDefaultFigure String
    | NamedFigure String (FigureFlag -> Bool)
    deriving (Eq, Ord, Show, Read)
-- function instances of Eq, Ord, Show, Read {{{
mapFromFunction f = Map.fromList [(k, f k) | k <- [minBound..maxBound]]

instance (Bounded k, Enum k, Ord k, Show k, Show v) => Show (k -> v) where
    showsPrec n = showsPrec n . mapFromFunction

instance (Bounded k, Enum k, Ord k, Read k, Read v) => Read (k -> v) where
    readsPrec n s = [((m Map.!), rest) | (m, rest) <- readsPrec n s, all (`Map.member` m) [minBound..]]

instance (Bounded k, Enum k, Ord k, Eq v) => Eq (k -> v) where
    f == g = mapFromFunction f == mapFromFunction g

instance (Bounded k, Enum k, Ord k, Ord v) => Ord (k -> v) where
    compare = comparing mapFromFunction
-- }}}
-- }}}
-- Move {{{
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
-- }}}
-- Setup {{{
data Setup stone = Setup {
    addBlack :: Set stone,
    addWhite :: Set stone,
    remove   :: Set Point,
    toPlay   :: Maybe Color
    } deriving (Eq, Ord, Show, Read)
emptySetup = Setup Set.empty Set.empty Set.empty Nothing
-- }}}
-- GameInfo {{{
data GameInfo ruleSet extra = GameInfo {
    rankBlack       :: Maybe Rank,
    rankWhite       :: Maybe Rank,
    date            :: Maybe [PartialDate], -- TODO: use Set PartialDate instead of Maybe [PartialDate]?
    round           :: Maybe Round,
    ruleSet         :: Maybe (RuleSet ruleSet),
    timeLimit       :: Maybe Rational,
    result          :: Maybe GameResult,
    freeform        :: Map GameInfoType String,
    otherGameInfo   :: extra
    } deriving (Eq, Ord, Show, Read)
emptyGameInfo = GameInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Map.empty ()

data GameInfoGo            = GameInfoGo             { handicap :: Maybe Integer, komi :: Maybe Rational }                                                                           deriving (Eq, Ord, Show, Read)
data GameInfoBackgammon    = GameInfoBackgammon     { match :: Maybe [MatchInfo] }                                                                                                  deriving (Eq, Ord, Show, Read)
data GameInfoLinesOfAction = GameInfoLinesOfAction  { initialPositionLOA :: InitialPosition, invertYAxis :: Bool, initialPlacement :: InitialPlacement }                            deriving (Eq, Ord, Show, Read)
data GameInfoHex           = GameInfoHex            { initialPositionHex :: Maybe () }                                                                                              deriving (Eq, Ord, Show, Read)
data GameInfoOcti          = GameInfoOcti           { squaresWhite :: Maybe [Point], squaresBlack :: Maybe [Point], prongs :: Integer, reserve :: Integer, superProngs :: Integer } deriving (Eq, Ord, Show, Read)
-- }}}
-- Annotation/Markup {{{
data Annotation extra = Annotation {
    comment     :: Maybe String,
    name        :: Maybe String,
    hotspot     :: Maybe Emphasis,
    value       :: Maybe Rational,
    judgment    :: Maybe (Judgment, Emphasis),
    otherAnnotation :: extra
    } deriving (Eq, Ord, Show, Read)
emptyAnnotation = Annotation Nothing Nothing Nothing Nothing Nothing ()

type AnnotationGo = Map Color (Set Point)

data Markup = Markup {
    marks       :: Map Point Mark,
    labels      :: Map Point String,
    arrows      :: Set (Point, Point),
    lines       :: Set (Point, Point),
    dim         :: Maybe (Set Point), -- inherit, default Set.empty
    visible     :: Maybe (Set Point), -- inherit, default to the whole board
    numbering   :: Maybe Numbering,   -- inherit, default Numbered
    figure      :: Maybe Figure       -- TODO: be extra careful when documenting this, especially the "NamedFigure" constructor
    } deriving (Eq, Ord, Show, Read)
emptyMarkup = Markup Map.empty Map.empty Set.empty Set.empty Nothing Nothing Nothing Nothing
-- }}}
-- Game/GameNode/GameTree {{{
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

data GameNode move stone ruleSet extraGameInfo extraAnnotation = GameNode {
    gameInfo    :: Maybe (GameInfo ruleSet extraGameInfo),
    action      :: Either (Setup stone) (Move move),
    annotation  :: Annotation extraAnnotation,
    markup      :: Markup,
    unknown     :: Map String [[Word8]]
    } deriving (Eq, Ord, Show, Read)
emptyGameNode = GameNode Nothing (Left emptySetup) emptyAnnotation emptyMarkup Map.empty

type NodeGo            = GameNode MoveGo  Point   RuleSetGo         GameInfoGo              AnnotationGo
type NodeBackgammon    = GameNode ()      ()      RuleSetBackgammon GameInfoBackgammon      ()
type NodeLinesOfAction = GameNode ()      ()      Void              GameInfoLinesOfAction   ()
type NodeHex           = GameNode ()      ()      Void              GameInfoHex             ()
type NodeOcti          = GameNode ()      ()      RuleSetOcti       GameInfoOcti            ()
type NodeOther         = GameNode [Word8] [Word8] Void              ()                      ()
-- }}}
