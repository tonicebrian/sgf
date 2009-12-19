-- boilerplate {{{
{-# LANGUAGE EmptyDataDecls #-}
-- | Types used to represent an SGF tree.  Whenever a data type is used by
-- exactly one other data type, there will be a \"see also\" link to its
-- containing type.
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
    NodeBackgammon, RuleSetBackgammon(..), GameInfoBackgammon,
    MatchInfo(..),

    -- ** Lines of Action
    NodeLinesOfAction, GameInfoLinesOfAction(..),
    InitialPosition(..), InitialPlacement(..),

    -- ** Hex
    NodeHex, GameInfoHex,
    ViewerSetting(..),

    -- ** Octi
    -- $octi
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
import Prelude hiding (round)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A type with no constructors used merely to indicate a lack of data.
data Void
instance Eq   Void where _ == _ = True
instance Ord  Void where compare _ _ = EQ
instance Read Void where readsPrec _ _ = []
instance Show Void where show _  = ""
-- }}}
-- GameType {{{
-- | See also 'GameTree'.  This enumeration is used for the GM property (see
-- <http://www.red-bean.com/sgf/properties.html#GM>).  The Enum instance
-- converts to and from the numeric game codes listed there.
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
-- | See also 'Game'.
type Application        = String
-- | See also 'Game'.
type Version            = String
-- | 0-indexed x/y coordinates that start at the top left
type Point              = (Integer, Integer)
-- | See also 'Game'.
type AutoMarkup         = Bool
-- }}}
-- enums {{{
-- | See also 'Move'.
data FuzzyBool          = Possibly  | Definitely    deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Emphasis           = Normal    | Strong        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Color              = Black     | White         deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'Rank'.
data Certainty          = Uncertain | Certain       deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'GameInfoLinesOfAction'.
data InitialPosition    = Beginning | End           deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'Rank'.  In addition to the standard \"kyu\" and \"dan\" ranks,
-- this also supports the non-standard (but common) \"pro\" ranks.
data RankScale          = Kyu | Dan | Pro           deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'Annotation'.
data Judgment           = GoodForWhite | GoodForBlack | Even | Unclear              deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'GameInfoLinesOfAction'.
data InitialPlacement   = Standard | ScrambledEggs | Parachute | Gemma | Custom     deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | See also 'Game'.
data VariationType
    = Children -- ^ Variations are stored in child nodes.
    | Siblings -- ^ Variations are stored in sibling nodes.
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | See also 'Markup'.  With the exception of 'Selected', the constructor
-- names describe a shape whose outline should be shown over the given point.
data Mark
    = Circle
    | X
    -- | The exact appearance of this kind of markup is not specified, though
    -- suggestions include darkening the colors on these points or inverting
    -- the colors on these points.
    | Selected
    | Square
    | Triangle
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | See also 'GameInfo', especially the 'freeform' field.
data GameInfoType
    -- | See also the BT and WT properties at
    -- <http://www.red-bean.com/sgf/properties.html#BT>
    = TeamName Color
    -- | See also the PB and PW properties at
    -- <http://www.red-bean.com/sgf/properties.html#PB>
    | PlayerName Color
    -- | The name of the person who annotated the game.  See also
    -- <http://www.red-bean.com/sgf/properties.html#AN>
    | Annotator
    -- | The name of the source, e.g. the title of the book this game came from.
    -- See also <http://www.red-bean.com/sgf/properties.html#SO>
    | Source
    -- | The name of the person or program who entered the game.  See also
    -- <http://www.red-bean.com/sgf/properties.html#US>
    | User
    -- | See also <http://www.red-bean.com/sgf/properties.html#CP>
    | Copyright
    -- | Background information or a summary of the game.  See also
    -- <http://www.red-bean.com/sgf/properties.html#GC>
    | Context
    -- | Where the game was played.  See also
    -- <http://www.red-bean.com/sgf/properties.html#PC>
    | Location
    -- | The name of the event or tournament at which the game occurred.
    -- Additional information about the game (e.g. that it was in the finals)
    -- should appear in the 'round' field, not here.  See also
    -- <http://www.red-bean.com/sgf/properties.html#EV>
    | Event
    -- | An easily-remembered moniker for the game.  See also
    -- <http://www.red-bean.com/sgf/properties.html#GN>
    | GameName
    -- | A description of the opening moves using the game's vernacular.  See
    -- also <http://www.red-bean.com/sgf/properties.html#ON>
    | Opening
    -- | The overtime rules.  See also
    -- <http://www.red-bean.com/sgf/properties.html#OT>
    | Overtime
    deriving (Eq, Ord, Show, Read)

-- | See also 'GameTree' and <http://www.red-bean.com/sgf/hex.html#IS>
data ViewerSetting
    = Tried     -- ^ Identify future moves that have been tried?
    | Marked    -- ^ Show good/bad move markings?
    | LastMove  -- ^ Identify the last cell played?
    | Headings  -- ^ Display column/row headings?
    | Lock      -- ^ Lock the game against new moves?
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | See also 'Markup'.
data Numbering
    = Unnumbered    -- ^ Don't print move numbers.
    | Numbered      -- ^ Print move numbers as they are.
    | Modulo100     -- ^ Subtract enough multiples of 100 from each move
                    -- number that the first labeled move is below 100.
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

allGameInfoTypes = [TeamName Black, TeamName White, PlayerName Black, PlayerName White, Annotator, Source, User, Copyright, Context, Location, Event, GameName, Opening, Overtime]
instance Enum GameInfoType where
    toEnum = (allGameInfoTypes !!)
    fromEnum t = fromJust $ findIndex (t==) allGameInfoTypes

instance Bounded GameInfoType where
    minBound = head allGameInfoTypes
    maxBound = last allGameInfoTypes
-- }}}
-- rulesets {{{
-- | See also 'RuleSet', 'GameInfo', and
-- <http://red-bean.com/sgf/properties.html#RU>
data RuleSetGo
    -- | American Go Association rules
    = AGA
    -- | Ing rules
    | GOE
    | Chinese | Japanese | NewZealand
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'RuleSet', 'GameInfo', and
-- <http://red-bean.com/sgf/backgammon.html#RU>
data RuleSetBackgammon
    -- | The Crawford rule is being used.
    = Crawford
    -- | This game /is/ the Crawford game.
    | CrawfordGame
    -- | The Jacoby rule is being used.
    | Jacoby
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'RuleSet', 'GameInfo', and <http://red-bean.com/sgf/octi.html#RU>
data RuleSetOcti        = OctiRuleSet MajorVariation (Set MinorVariation)           deriving (Eq, Ord, Show, Read)
-- | See also 'RuleSetOcti'.
data MajorVariation     = Full | Fast | Kids                                        deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'RuleSetOcti'.
data MinorVariation     = Edgeless | Superprong | OtherMinorVariation String        deriving (Eq, Ord, Show, Read)
-- | See also 'GameInfo'.  Typical values for the @a@ type variable are
-- 'RuleSetGo', 'RuleSetBackgammon', and 'RuleSetOcti'.  For games where the
-- valid values of the ruleset field is not specified, the @a@ type variable
-- will be 'Void' to ensure that all rulesets are specified as a 'String'.
data RuleSet a          = Known !a | OtherRuleSet String                            deriving (Eq, Ord, Show, Read)
-- }}}
-- misc types {{{
-- | See also 'GameResult'.  Games that end normally use @Score@ if there is a
-- natural concept of score differential for that game and @OtherWinType@ if
-- not.
data WinType            = Score Rational | Resign | Time | Forfeit | OtherWinType   deriving (Eq, Ord, Show, Read)
-- | See also 'Move'.
data Quality            = Bad Emphasis | Doubtful | Interesting | Good Emphasis     deriving (Eq, Ord, Show, Read)

-- | See also 'GameInfo'.
data GameResult
    = Draw | Void | Unknown
    | Win Color WinType -- ^ The first argument is the color of the winner.
    deriving (Eq, Ord, Show, Read)

-- | See also 'GameInfo', especially the 'rankBlack' and 'rankWhite' fields.
-- The @Eq@ and @Ord@ instances are the derived ones, and should not be mistaken
-- for semantic equality or ordering.
data Rank
    -- | Ranked in one of the standard ways.  Most SGF generators specify
    -- the certainty only when it is @Uncertain@.  Therefore, it may be
    -- reasonable to treat @Nothing@ and @Just Certain@ identically.
    = Ranked Integer RankScale (Maybe Certainty)
    -- | Any rank that does not fall in the standard categories.  This field
    -- must not contain newlines.
    | OtherRank String
    deriving (Eq, Ord, Show, Read)

-- | See also 'GameInfo'.
data Round
    -- | Only a round number is given.
    = SimpleRound    Integer
    -- | Both a round number and a type, like \"final\", \"playoff\", or
    -- \"league\".
    | FormattedRound Integer String
    -- | Round information in an unknown format.
    | OtherRound             String
    deriving (Eq, Ord, Show, Read)

-- | See also 'GameInfoBackgammon' and
-- <http://red-bean.com/sgf/backgammon.html#MI>
data MatchInfo
    -- | The number of points in this match.
    = Length           Integer
    -- | The (1-indexed) number of the game within this match.
    | GameNumber       Integer
    -- | The score at the beginning of the game.
    | StartScore Color Integer
    -- | An unknown piece of match information.
    | OtherMatchInfo String String
    deriving (Eq, Ord, Show, Read)

-- | See also 'GameInfo'.
data PartialDate
    = Year  { year :: Integer }
    | Month { year :: Integer, month :: Integer }
    | Day   { year :: Integer, month :: Integer, day :: Integer }
    deriving (Eq, Ord, Show, Read)
-- }}}
-- Figure {{{
-- FigureFlag {{{
-- | See also 'Figure'.
data FigureFlag
    -- | Show coordinates around the edges of the board.
    = Coordinates
    -- | Show the diagram's name.
    | Name
    -- | List moves that can't be shown in the diagram as text.
    | HiddenMoves
    -- | Remove captured stones from the diagram.
    | RemoveCaptures
    -- | Show hoshi dots.
    | Hoshi
    deriving (Eq, Ord, Show, Read, Bounded)

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
-- | See also 'Markup'.
data Figure
    -- | Unnamed figure using the application default settings.
    = DefaultFigure
    -- | Named figure using the application default settings.
    | NamedDefaultFigure String
    -- | Named figure that overrides the application's figure settings.
    | NamedFigure String (FigureFlag -> Bool)
    deriving (Eq, Ord, Show, Read)
-- function instances of Eq, Ord, Show, Read {{{
mapFromFunction f = Map.fromList [(k, f k) | k <- [minBound..maxBound]]

-- TODO: remove Ord k constraints
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
-- | See also 'GameNode'.
data Move move = Move {
    -- | The given move should be executed, whether it is illegal or not.
    -- See also the B and W properties at
    -- <http://www.red-bean.com/sgf/properties.html#B>
    move                :: Maybe (Color, move),
    -- | When set to 'Definitely', the current move is acknowledged to be
    -- illegal.  When set to 'Possibly', the move may be legal or illegal.
    -- See also <http://www.red-bean.com/sgf/properties.html#KO>
    illegal             :: FuzzyBool,
    -- | When @Just@, set the current move number.  See also
    -- <http://www.red-bean.com/sgf/properties.html#MN>
    number              :: Maybe Integer,
    -- | An annotation telling the quality of the current move.  This
    -- annotation makes no bigger-picture positional judgments; for those,
    -- see 'Annotation'.  See also the BM, DO, IT, and TE properties at
    -- <http://www.red-bean.com/sgf/properties.html#BM>
    quality             :: Maybe Quality,
    -- | Time remaining, in seconds, for the black player after this move
    -- was made.  See also <http://www.red-bean.com/sgf/properties.html#BL>
    timeBlack           :: Maybe Rational,
    -- | Time remaining, in seconds, for the white player after this move
    -- was made.  See also <http://www.red-bean.com/sgf/properties.html#WL>
    timeWhite           :: Maybe Rational,
    -- | Number of overtime moves left for the black player after this move
    -- was made.  See also <http://www.red-bean.com/sgf/properties.html#OB>
    overtimeMovesBlack  :: Maybe Integer,
    -- | Number of overtime moves left for the white player after this move
    -- was made.  See also <http://www.red-bean.com/sgf/properties.html#OW>
    overtimeMovesWhite  :: Maybe Integer
    } deriving (Eq, Ord, Show, Read)

emptyMove :: Move move
emptyMove = Move Nothing Possibly Nothing Nothing Nothing Nothing Nothing Nothing

-- | See also 'NodeGo' and 'Game'.
data MoveGo = Pass | Play Point deriving (Eq, Ord, Show, Read)
-- }}}
-- Setup {{{
-- | See also 'GameNode'.  'Setup' nodes are distinct from 'Move' nodes in that
-- they need not correspond to any natural part of the game, and game rules
-- (e.g. for capture) are not applied after executing 'Setup' nodes.  They can
-- be used for any non-standard changes to the game board or to create illegal
-- board positions.  The locations specified in the @addBlack@, @addWhite@, and
-- @remove@ fields must be pairwise disjoint.
data Setup stone = Setup {
    -- | This node adds the given black pieces to the board; if the board
    -- before this setup node had any pieces at the locations given by this
    -- field, they are overridden.  See also
    -- <http://www.red-bean.com/sgf/properties.html#AB>
    addBlack :: Set stone,
    -- | This node adds the given white pieces to the board; if the board
    -- before this setup node had any pieces at the locations given by this
    -- field, they are overridden.  See also
    -- <http://www.red-bean.com/sgf/properties.html#AW>
    addWhite :: Set stone,
    -- | This node specifies locations of the board to clear.  See also
    -- <http://www.red-bean.com/sgf/properties.html#AE>
    remove   :: Set Point,
    -- | Specify which player should move next.  See also
    -- <http://www.red-bean.com/sgf/properties.html#PL>
    toPlay   :: Maybe Color
    } deriving (Eq, Ord, Show, Read)

emptySetup :: Setup stone
emptySetup = Setup Set.empty Set.empty Set.empty Nothing
-- }}}
-- GameInfo {{{
-- | See also 'GameNode'.  Each individual game may have at most one node with
-- associated game info.  If it has such a node, it must occur at the first node
-- where that game is distinguishable from all of the other games in the tree.
data GameInfo ruleSet extra = GameInfo {
    -- | The strength of the black player.  See also
    -- <http://www.red-bean.com/sgf/properties.html#BR>
    rankBlack       :: Maybe Rank,
    -- | The strength of the white player.  See also
    -- <http://www.red-bean.com/sgf/properties.html#WR>
    rankWhite       :: Maybe Rank,
    -- | When the game was played.  An empty set indicates that no date
    -- information is available.  See also
    -- <http://www.red-bean.com/sgf/properties.html#DT>
    date            :: Set PartialDate,
    -- | The round number (for games played in a tournament).  See also
    -- <http://www.red-bean.com/sgf/properties.html#RO>
    round           :: Maybe Round,
    -- | The ruleset used for this game.  See also
    -- <http://www.red-bean.com/sgf/properties.html#RU>
    ruleSet         :: Maybe (RuleSet ruleSet),
    -- | The time limit of the game in seconds.  See also
    -- <http://www.red-bean.com/sgf/properties.html#TM>
    timeLimit       :: Maybe Rational,
    -- | How the game ended.  See also
    -- <http://www.red-bean.com/sgf/properties.html#RE>
    result          :: Maybe GameResult,
    -- | Miscellaneous properties with no prescribed format.
    freeform        :: Map GameInfoType String,
    -- | Certain game types specify additional informational properties, which
    -- are stored here.
    otherGameInfo   :: extra
    } deriving (Eq, Ord, Show, Read)

emptyGameInfo :: GameInfo ruleSet ()
emptyGameInfo = GameInfo Nothing Nothing Set.empty Nothing Nothing Nothing Nothing Map.empty ()

-- | See also 'NodeGo' and the 'otherGameInfo' field of 'GameInfo'.
data GameInfoGo = GameInfoGo {
    -- | Specifying this does not automatically add stones to the board; a
    -- 'Setup' node with a non-empty 'addBlack' field should be specified before
    -- any 'Move' nodes.  See also <http://red-bean.com/sgf/go.html#HA>
    handicap :: Maybe Integer,
    -- | See also <http://red-bean.com/sgf/go.html#KM>
    komi     :: Maybe Rational
    } deriving (Eq, Ord, Show, Read)

-- | See also 'NodeBackgammon' and the 'otherGameInfo' field of 'GameInfo'.  An
-- empty list indicates that no match information was specified.  The order of
-- the list is not significant, and there should be only one value of any given
-- kind of 'MatchInfo'.  See also <http://red-bean.com/sgf/backgammon.html#MI>
type GameInfoBackgammon = [MatchInfo]

-- | See also 'NodeLinesOfAction' and the 'otherGameInfo' field of 'GameInfo'.
data GameInfoLinesOfAction = GameInfoLinesOfAction {
    -- | When this field is 'Beginning', the viewer should initially show the
    -- board position after setup but before any moves.  (When 'End', the
    -- viewer should display the final position.)  See also
    -- <http://www.red-bean.com/sgf/loa.html#IP>
    initialPositionLOA  :: InitialPosition,
    -- | When this field is 'True', the board should be displayed with numbers
    -- increasing from the bottom to the top of the screen.  (When 'False', the
    -- numbers should be decreasing.)  See also
    -- <http://www.red-bean.com/sgf/loa.html#IY>
    invertYAxis         :: Bool,
    -- | The initial placement of pieces and rule variation.  See also
    -- <http://www.red-bean.com/sgf/loa.html#SU>
    initialPlacement    :: InitialPlacement
    } deriving (Eq, Ord, Show, Read)

-- | See also 'NodeHex' and the 'otherGameInfo' field of 'GameInfo'.  The
-- specification says that trees representing Hex games will mark which
-- position the viewer should initially show by setting this field to 'True'.
-- I think this is probably an error in the specification; there is an obvious
-- conflict between the requirement to put all game information at the first
-- node where a game is uniquely identifiable and the requirement to have a
-- game-information property at the location you want to view first (whenever
-- these two nodes are not the same node, of course).  For this reason, Hex
-- game trees may have paths containing two nodes whose game information is not
-- 'Nothing'.  See also <http://www.red-bean.com/sgf/hex.html#IP>
type GameInfoHex = Bool

-- $octi
-- For Octi, 'Black' always refers to the first player and 'White' always
-- refers to the second player, regardless of the colors of the actual pieces
-- used by the first and second players.

-- | See also 'NodeOcti' and the 'otherGameInfo' field of 'GameInfo'.
data GameInfoOcti = GameInfoOcti {
    -- | Black should be set up with one empty pod on each of these points.  An
    -- empty set indicates that this property was not specified.  See also
    -- <http://www.red-bean.com/sgf/octi.html#BO>
    squaresWhite    :: Set Point,
    -- | White should be set up with one empty pod on each of these points.  An
    -- empty set indicates that this property was not specified.  See also
    -- <http://www.red-bean.com/sgf/octi.html#WO>
    squaresBlack    :: Set Point,
    -- | How many prongs each player starts with.  See also
    -- <http://www.red-bean.com/sgf/octi.html#NP>
    prongs          :: Integer,
    -- | How many pods each player has in reserve to start with.  See also
    -- <http://www.red-bean.com/sgf/octi.html#NR>
    reserve         :: Integer,
    -- | How many superprongs each player starts with.  See also
    -- <http://www.red-bean.com/sgf/octi.html#NS>
    superProngs     :: Integer
    } deriving (Eq, Ord, Show, Read)
-- }}}
-- Annotation/Markup {{{
-- | See also 'GameNode'.
data Annotation extra = Annotation {
    -- | Free-form text describing the current node.  See also
    -- <http://www.red-bean.com/sgf/properties.html#C>
    comment     :: Maybe String,
    -- | A very short description of the node.  Must not contain newlines.
    -- See also <http://www.red-bean.com/sgf/properties.html#N>
    name        :: Maybe String,
    -- | When @Just@, this node contains something interesting.  Viewers
    -- should show a message.  See also
    -- <http://www.red-bean.com/sgf/properties.html#HO>
    hotspot     :: Maybe Emphasis,
    -- | A quantitative full-board positional judgment.  Positive values are
    -- good for black, negative for white.  See also
    -- <http://www.red-bean.com/sgf/properties.html#V>
    value       :: Maybe Rational,
    -- | A qualitative full-board positional judgment.  See also the GB, GW,
    -- DM, and UC properties at
    -- <http://www.red-bean.com/sgf/properties.html#DM>
    judgment    :: Maybe (Judgment, Emphasis),
    -- | Game-specific annotations.
    otherAnnotation :: extra
    } deriving (Eq, Ord, Show, Read)

emptyAnnotation :: Annotation ()
emptyAnnotation = Annotation Nothing Nothing Nothing Nothing Nothing ()

-- | See also 'NodeGo' and the 'otherAnnotation' field of 'Annotation'.  This
-- specifies which points are considered territory for each player.  See also
-- the TB and TW properties at <http://red-bean.com/sgf/go.html#TB>
type AnnotationGo = Map Color (Set Point)

-- | See also 'GameNode'.  Presumably, no arrow in the @arrows@ field should
-- exactly overlap a line specified in the @lines@ field; however, this is not
-- explicitly made illegal by the SGF spec.  Note that some fields are marked
-- \"inherit\".  These inheritances are not explicitly tracked; @Nothing@ values
-- indicate that the correct interpretation depends on the node's ancestors, or
-- on the default if no ancestor has a @Just@ value in this field.
data Markup = Markup {
    -- | See also the CR, MA, SL, SQ, and TR properties at
    -- <http://www.red-bean.com/sgf/properties.html#CR>
    marks       :: Map Point Mark,
    -- | Typically, the @String@s will be single letters, but that is not
    -- guaranteed.  Labels must not contain newlines.  See also
    -- <http://www.red-bean.com/sgf/properties.html#LB>
    labels      :: Map Point String,
    -- | Arrows must not start and end at the same point.  See also
    -- <http://www.red-bean.com/sgf/properties.html#AR>
    arrows      :: Set (Point, Point),
    -- | Lines must not start and end at the same point.  Lines must not be
    -- repeated; the parser guarantees this by only including pairs @(p, q)@
    -- in which @p \< q@.  See also
    -- <http://www.red-bean.com/sgf/properties.html#LN>
    lines       :: Set (Point, Point),
    -- | Shade out (exactly) the given points.  This property is inherited,
    -- defaulting to @Set.empty@.  See also
    -- <http://www.red-bean.com/sgf/properties.html#DD>
    dim         :: Maybe (Set Point),
    -- | Make (exactly) the given points visible; do not draw any of the
    -- other points.  This property is inherited, defaulting to the entire
    -- board, and @Set.empty@ resets to the default.  See also
    -- <http://www.red-bean.com/sgf/properties.html#VW>
    visible     :: Maybe (Set Point),
    -- | How move numbers should be printed on the board.  This property is
    -- inherited, defaulting to @Numbered@.  See also
    -- <http://www.red-bean.com/sgf/properties.html#PM>
    numbering   :: Maybe Numbering,
    -- | When @Just@, a new diagram should begin at this move.  See also
    -- <http://www.red-bean.com/sgf/properties.html#FG>
    figure      :: Maybe Figure
    } deriving (Eq, Ord, Show, Read)

emptyMarkup :: Markup
emptyMarkup = Markup Map.empty Map.empty Set.empty Set.empty Nothing Nothing Nothing Nothing
-- }}}
-- Game/GameNode/GameTree {{{
-- | See also 'Collection'.
data Game = Game {
    -- | The name and version number of the application used to create this
    -- game.  The version number must be in a format that allows ordinary
    -- string comparison to tell which version is higher or lower. Neither
    -- the application name nor the version number may have newlines.  See
    -- also <http://www.red-bean.com/sgf/properties.html#AP>
    application     :: Maybe (Application, Version),
    -- | The first argument tells whether children (False) or siblings
    -- (True) are variations; the second argument tells whether or not to
    -- show board markup when variations are available.  See also
    -- <http://www.red-bean.com/sgf/properties.html#ST>
    variationType   :: Maybe (VariationType, AutoMarkup),
    -- | The size of the board.  For games with a default board size, this
    -- is guaranteed to be a @Just@.  See also
    -- <http://www.red-bean.com/sgf/properties.html#SZ>
    size            :: Maybe (Integer, Integer),
    -- | The actual game tree.
    tree            :: GameTree
    } deriving (Eq, Show, Read)

-- | See also 'Game'.
data GameTree
    = TreeGo                          TreeGo
    | TreeBackgammon                  TreeBackgammon
    | TreeLinesOfAction               TreeLinesOfAction
    -- | Applications can store and read settings in the first argument
    -- here.  This got totally shoehorned into the spec by some particular
    -- viewer, I'm sure, but it's in the spec now, so there we go.  See also
    -- <http://www.red-bean.com/sgf/hex.html#IS>
    | TreeHex [(ViewerSetting, Bool)] TreeHex
    | TreeOcti                        TreeOcti
    | TreeOther GameType              TreeOther
    deriving (Eq, Show, Read)

-- | See also 'GameTree'.
type TreeGo            = Tree NodeGo
-- | See also 'GameTree'.
type TreeBackgammon    = Tree NodeBackgammon
-- | See also 'GameTree'.
type TreeLinesOfAction = Tree NodeLinesOfAction
-- | See also 'GameTree'.
type TreeHex           = Tree NodeHex
-- | See also 'GameTree'.
type TreeOcti          = Tree NodeOcti
-- | See also 'GameTree'.
type TreeOther         = Tree NodeOther

-- | See also 'GameTree'.
data GameNode move stone ruleSet extraGameInfo extraAnnotation = GameNode {
    -- | All properties with propertytype game-info.  There must be only one
    -- @Just@ on any path within a 'GameTree'.
    gameInfo    :: Maybe (GameInfo ruleSet extraGameInfo),
    -- | All properties with propertytype setup or move.
    action      :: Either (Setup stone) (Move move),
    -- | Positional judgments and comments (as opposed to judgments of
    -- particular moves).  All properties covered in the \"Node annotation\"
    -- section.
    annotation  :: Annotation extraAnnotation,
    -- | How a node should be displayed.  All properties covered in the
    -- \"Markup\" and \"Miscellaneous\" sections.
    markup      :: Markup,
    -- | Unspecified properties.  The keys in the map must contain only
    -- the characters A-Z (and must be upper-case).  The values in the map
    -- may be more or less arbitrary, but any occurrence of the ASCII byte
    -- \']\' must be preceded by an odd number of copies of the ASCII byte
    -- \'\\\'.  See also <http://www.red-bean.com/sgf/sgf4.html#2.2>
    unknown     :: Map String [[Word8]]
    } deriving (Eq, Ord, Show, Read)

emptyGameNode :: GameNode move stone ruleSet extraGameInfo ()
emptyGameNode = GameNode Nothing (Left emptySetup) emptyAnnotation emptyMarkup Map.empty

-- | See also 'TreeGo' and 'Game'.
type NodeGo            = GameNode MoveGo  Point   RuleSetGo         GameInfoGo              AnnotationGo
-- | See also 'TreeBackgammon' and 'Game'.
type NodeBackgammon    = GameNode ()      ()      RuleSetBackgammon GameInfoBackgammon      ()
-- | See also 'TreeLinesOfAction' and 'Game'.
type NodeLinesOfAction = GameNode ()      ()      Void              GameInfoLinesOfAction   ()
-- | See also 'TreeHex' and 'Game'.
type NodeHex           = GameNode ()      ()      Void              GameInfoHex             ()
-- | See also 'TreeOcti' and 'Game'.
type NodeOcti          = GameNode ()      ()      RuleSetOcti       GameInfoOcti            ()
-- | See also 'TreeOther' and 'Game'.
type NodeOther         = GameNode [Word8] [Word8] Void              ()                      ()
-- }}}
