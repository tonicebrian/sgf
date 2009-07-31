module SGF.Types (
    Collection,
    Application,
    Version,
    GameType(..),
    VariationType(..),
    AutoMarkup,
    Game(..),
    Emphasis(..),
    Color(..),
    GameNode(..)
) where

import Data.List
import Data.Maybe
import Data.Tree

type Collection = [Game]
type Application = String
type Version = String

data GameType =
    Go | Othello | Chess | Gomoku | NineMen'sMorris |
    Backgammon | ChineseChess | Shogi | LinesOfAction | Ataxx |
    Hex | Jungle | Neutron | Philosopher'sFootball |
    Quadrature | Trax | Tantrix | Amazons | Octi | Gess |
    Twixt | Zertz | Plateau | Yinsh | Punct | Gobblet | Hive |
    Exxit | Hnefatal | Kuba | Tripples | Chase | TumblingDown |
    Sahara | Byte | Focus | Dvonn | Tamsk | Gipf | Kropki
    deriving (Eq, Ord, Show, Read)

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

data VariationType = Children | Siblings deriving (Eq, Ord, Enum, Show, Read)
type AutoMarkup    = Bool

data Game = Game {
    application     :: Maybe (Application, Version),
    gameType        :: Maybe GameType,
    variationType   :: Maybe (VariationType, AutoMarkup),
    size            :: Maybe (Integer, Integer),
    header          :: GameHeader,
    game            :: Tree GameNode
    } deriving (Eq, Show, Read)

data Emphasis   = Normal | Strong deriving (Eq, Ord, Enum, Show, Read)
data Color      = Black  | White  deriving (Eq, Ord, Enum, Show, Read)
data GameNode   = GameNode   deriving (Eq, Ord, Show, Read) -- TODO
data GameHeader = GameHeader deriving (Eq, Ord, Show, Read) -- TODO
