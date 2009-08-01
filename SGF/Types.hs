module SGF.Types where

import Data.List
import Data.Maybe
import Data.Tree

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

type Collection       = [Game]
type Application      = String
type Version          = String
type Point            = (Integer, Integer) -- 0-indexed
type AutoMarkup       = Bool
data VariationType    = Children  | Siblings deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Emphasis         = Normal    | Strong   deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Color            = Black     | White    deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RankScale        = Kyu       | Dan      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Certainty        = Uncertain | Certain  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPosition  = Beginning | End      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ViewerSetting    = Tried | Marked | LastMove | Headings | Lock             deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPlacement = Standard | ScrambledEggs | Parachute | Gemma | Custom   deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MajorVariation   = Full | Fast | Kids                                      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MinorVariation   = Edgeless | Superprong | OtherMinorVariation String      deriving (Eq, Ord, Show, Read)
data WinType          = Score Rational | Resign | Time | Forfeit                deriving (Eq, Ord, Show, Read)
data GameResult       = Draw | Void | Unknown | Win Color WinType               deriving (Eq, Ord, Show, Read)
data Round            = FormattedRound Integer String | UnformattedRound String deriving (Eq, Ord, Show, Read)
data Rank             = Ranked RankScale (Maybe Certainty) | OtherRank String   deriving (Eq, Ord, Show, Read)

data MatchInfo = Length           Integer
               | GameNumber       Integer
               | StartScore Color Integer
               | OtherMatchInfo String String
    deriving (Eq, Ord, Show, Read)

data RuleSet = AGA | GOE | Japanese | NewZealand           -- Go
             | Crawford | CrawfordGame | Jacoby            -- Backgammon
             | OctiRuleSet MajorVariation [MinorVariation] -- Octi, obviously
             | OtherRuleSet String
    deriving (Eq, Ord, Show, Read)

data PartialDate
    = Year  { year :: Integer }
    | Month { year :: Integer, month :: Integer }
    | Day   { year :: Integer, month :: Integer, day :: Integer }
    deriving (Eq, Ord, Show, Read)

data GeneralHeader = GeneralHeader {
    application     :: Maybe (Application, Version),
    gameType        :: GameType,
    variationType   :: Maybe (VariationType, AutoMarkup),
    size            :: Maybe (Integer, Integer)
    } deriving (Eq, Ord, Show, Read)

data SpecificHeader = HexHeader {
    initialSetting  :: [(ViewerSetting, Bool)]
    } deriving (Eq, Ord, Show, Read)

data Header = Header {
    generalHeader  :: GeneralHeader,
    specificHeader :: Maybe SpecificHeader
    } deriving (Eq, Ord, Show, Read)

data GeneralGameInfo = GeneralGameInfo {
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
    ruleset         :: Maybe RuleSet,
    timeLimit       :: Maybe Rational,
    result          :: Maybe GameResult
    } deriving (Eq, Ord, Show, Read)

data SpecificGameInfo
    = GameInfoGo                { handicap :: Maybe Integer, komi :: Maybe Rational }
    | GameInfoBackgammon        { match :: Maybe [MatchInfo]  }
    | GameInfoLinesOfAction     { initialPositionLOA :: InitialPosition, invertYAxis :: Bool, initialPlacement :: InitialPlacement }
    | GameInfoHex               { initialPositionHex :: Maybe () }
    | GameInfoOcti              { squaresWhite :: Maybe [Point], squaresBlack :: Maybe [Point], prongs :: Integer, reserve :: Integer, superProngs :: Integer }
    deriving (Eq, Ord, Show, Read)

data GameInfo = GameInfo {
    generalGameInfo  :: GeneralGameInfo,
    specificGameInfo :: Maybe SpecificGameInfo
    } deriving (Eq, Ord, Show, Read)

data Game = Game deriving (Eq, Ord, Show, Read) -- TODO
