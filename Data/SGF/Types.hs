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
-- | This enumeration is used for the GM property (see <http://www.red-bean.com/sgf/properties.html#GM>).  The Enum instance converts to and from the numeric game codes listed there.  See also 'GameTree'.
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
-- | See also 'Move'.
data FuzzyBool          = Possibly  | Definitely    deriving (Eq, Ord, Show, Read, Enum, Bounded)
data VariationType      = Children  | Siblings      deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Emphasis           = Normal    | Strong        deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Color              = Black     | White         deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Certainty          = Uncertain | Certain       deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPosition    = Beginning | End           deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RankScale          = Kyu | Dan | Pro           deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'Annotation'.
data Judgment           = GoodForWhite | GoodForBlack | Even | Unclear              deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | See also 'Markup'.
data Mark               = Circle | X | Selected | Square | Triangle                 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data InitialPlacement   = Standard | ScrambledEggs | Parachute | Gemma | Custom     deriving (Eq, Ord, Show, Read, Enum, Bounded)
data GameInfoType       = TeamName Color | PlayerName Color | Annotator | Source | User | Copyright | Context | Location | Event | GameName | Opening | Overtime
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
    | Modulo100     -- ^ Subtract enough multiples of 100 from each move number that the first labeled move is below 100.
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
-- | See also 'Move'.
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
    -- | Named figure that overrides the applications figure settings.
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
data Move move = Move {
    -- | The given move should be executed, whether it is illegal or not.  See also the B and W properties at <http://www.red-bean.com/sgf/properties.html#B>
    move                :: Maybe (Color, move),
    -- | When set to 'Definitely', the current move is acknowledged to be illegal.  When set to 'Possibly', the move may be legal or illegal.  See also <http://www.red-bean.com/sgf/properties.html#KO>
    illegal             :: FuzzyBool,
    -- | When @Just@, set the current move number.  See also <http://www.red-bean.com/sgf/properties.html#MN>
    number              :: Maybe Integer,
    -- | An annotation telling the quality of the current move.  This annotation makes no bigger-picture positional judgments; for those, see 'Annotation'.  See also the BM, DO, IT, and TE properties at <http://www.red-bean.com/sgf/properties.html#BM>
    quality             :: Maybe Quality,
    -- | Time remaining, in seconds, for the black player after this move was made.  See also <http://www.red-bean.com/sgf/properties.html#BL>
    timeBlack           :: Maybe Rational,
    -- | Time remaining, in seconds, for the white player after this move was made.  See also <http://www.red-bean.com/sgf/properties.html#WL>
    timeWhite           :: Maybe Rational,
    -- | Number of overtime moves left for the black player after this move was made.  See also <http://www.red-bean.com/sgf/properties.html#OB>
    overtimeMovesBlack  :: Maybe Integer,
    -- | Number of overtime moves left for the white player after this move was made.  See also <http://www.red-bean.com/sgf/properties.html#OW>
    overtimeMovesWhite  :: Maybe Integer
    } deriving (Eq, Ord, Show, Read)

emptyMove :: Move move
emptyMove = Move Nothing Possibly Nothing Nothing Nothing Nothing Nothing Nothing

data MoveGo = Pass | Play Point deriving (Eq, Ord, Show, Read)
-- }}}
-- Setup {{{
-- | 'Setup' nodes are distinct from 'Move' nodes in that they need not correspond to any natural part of the game, and game rules (e.g. for capture) are not applied after executing 'Setup' nodes.  They can be used for any non-standard changes to the game board or to create illegal board positions.
-- The locations specified in the @addBlack@, @addWhite@, and @remove@ fields must be pairwise disjoint.
data Setup stone = Setup {
    -- | This node adds the given black pieces to the board; if the board before this setup node had any pieces at the locations given by this field, they are overridden.  See also <http://www.red-bean.com/sgf/properties.html#AB>
    addBlack :: Set stone,
    -- | This node adds the given white pieces to the board; if the board before this setup node had any pieces at the locations given by this field, they are overridden.  See also <http://www.red-bean.com/sgf/properties.html#AW>
    addWhite :: Set stone,
    -- | This node specifies locations of the board to clear.  See also <http://www.red-bean.com/sgf/properties.html#AE>
    remove   :: Set Point,
    -- | Specify which player should move next.  See also <http://www.red-bean.com/sgf/properties.html#PL>
    toPlay   :: Maybe Color
    } deriving (Eq, Ord, Show, Read)

emptySetup :: Setup stone
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

emptyGameInfo :: GameInfo ruleSet ()
emptyGameInfo = GameInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Map.empty ()

data GameInfoGo            = GameInfoGo             { handicap :: Maybe Integer, komi :: Maybe Rational }                                                                           deriving (Eq, Ord, Show, Read)
data GameInfoBackgammon    = GameInfoBackgammon     { match :: Maybe [MatchInfo] }                                                                                                  deriving (Eq, Ord, Show, Read)
data GameInfoLinesOfAction = GameInfoLinesOfAction  { initialPositionLOA :: InitialPosition, invertYAxis :: Bool, initialPlacement :: InitialPlacement }                            deriving (Eq, Ord, Show, Read)
data GameInfoHex           = GameInfoHex            { initialPositionHex :: Maybe () }                                                                                              deriving (Eq, Ord, Show, Read)
data GameInfoOcti          = GameInfoOcti           { squaresWhite :: Maybe [Point], squaresBlack :: Maybe [Point], prongs :: Integer, reserve :: Integer, superProngs :: Integer } deriving (Eq, Ord, Show, Read)
-- }}}
-- Annotation/Markup {{{
data Annotation extra = Annotation {
    -- | Free-form text describing the current node.  See also <http://www.red-bean.com/sgf/properties.html#C>
    comment     :: Maybe String,
    -- | A very short description of the node.  Must not contain newlines.  See also <http://www.red-bean.com/sgf/properties.html#N>
    name        :: Maybe String,
    -- | When @Just@, this node contains something interesting.  Viewers should show a message.  See also <http://www.red-bean.com/sgf/properties.html#HO>
    hotspot     :: Maybe Emphasis,
    -- | A quantitative full-board positional judgment.  Positive values are good for black, negative for white.  See also <http://www.red-bean.com/sgf/properties.html#V>
    value       :: Maybe Rational,
    -- | A qualitative full-board positional judgment.  See also the GB, GW, DM, and UC properties at <http://www.red-bean.com/sgf/properties.html#DM>
    judgment    :: Maybe (Judgment, Emphasis),
    -- | Game-specific annotations.
    otherAnnotation :: extra
    } deriving (Eq, Ord, Show, Read)

emptyAnnotation :: Annotation ()
emptyAnnotation = Annotation Nothing Nothing Nothing Nothing Nothing ()

type AnnotationGo = Map Color (Set Point)

-- | Presumably, no arrow in the @arrows@ field should exactly overlap a line specified in the @lines@ field; however, this is not explicitly made illegal by the SGF spec.
-- Note that some fields are marked \"inherit\".  These inheritances are not explicitly tracked; @Nothing@ values indicate that the correct interpretation depends on the node's ancestors, or on the default if no ancestor has a @Just@ value in this field.
data Markup = Markup {
    -- | See also the CR, MA, SL, SQ, and TR properties at <http://www.red-bean.com/sgf/properties.html#CR>
    marks       :: Map Point Mark,
    -- | Typically, the @String@s will be single letters, but that is not guaranteed.  Labels must not contain newlines.  See also <http://www.red-bean.com/sgf/properties.html#LB>
    labels      :: Map Point String,
    -- | Arrows must not start and end at the same point.  See also <http://www.red-bean.com/sgf/properties.html#AR>
    arrows      :: Set (Point, Point),
    -- | Lines must not start and end at the same point.  Lines must not be repeated; the parser guarantees this by only including pairs @(p, q)@ in which @p \< q@.  See also <http://www.red-bean.com/sgf/properties.html#LN>
    lines       :: Set (Point, Point),
    -- | Shade out (exactly) the given points.  This property is inherited, defaulting to @Set.empty@.  See also <http://www.red-bean.com/sgf/properties.html#DD>
    dim         :: Maybe (Set Point),
    -- | Make (exactly) the given points visible; do not draw any of the other points.  This property is inherited, defaulting to the entire board, and @Set.empty@ resets to the default.  See also <http://www.red-bean.com/sgf/properties.html#VW>
    visible     :: Maybe (Set Point),
    -- | How move numbers should be printed on the board.  This property is inherited, defaulting to @Numbered@.  See also <http://www.red-bean.com/sgf/properties.html#PM>
    numbering   :: Maybe Numbering,
    -- | When @Just@, a new diagram should begin at this move.  See also <http://www.red-bean.com/sgf/properties.html#FG>
    figure      :: Maybe Figure
    } deriving (Eq, Ord, Show, Read)

emptyMarkup :: Markup
emptyMarkup = Markup Map.empty Map.empty Set.empty Set.empty Nothing Nothing Nothing Nothing
-- }}}
-- Game/GameNode/GameTree {{{
data Game = Game {
    -- | The name and version number of the application used to create this game.  The version number must be in a format that allows ordinary string comparison to tell which version is higher or lower. See also <http://www.red-bean.com/sgf/properties.html#AP>
    application     :: Maybe (Application, Version),
    -- | The first argument tells whether children (False) or siblings (True) are variations; the second argument tells whether or not to show board markup when variations are available.  See also <http://www.red-bean.com/sgf/properties.html#ST>
    variationType   :: Maybe (VariationType, AutoMarkup),
    -- | The size of the board.  For games with a default board size, this is guaranteed to be a @Just@.  See also <http://www.red-bean.com/sgf/properties.html#SZ>
    size            :: Maybe (Integer, Integer),
    -- | The actual game tree.
    tree            :: GameTree
    } deriving (Eq, Show, Read)

data GameTree
    = TreeGo                          TreeGo
    | TreeBackgammon                  TreeBackgammon
    | TreeLinesOfAction               TreeLinesOfAction
    -- | Applications can store and read settings in the first argument here.  This got totally shoehorned into the spec by some particular viewer, I'm sure, but it's in the spec now, so there we go.  See also <http://www.red-bean.com/sgf/hex.html#IS>
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
    -- | All properties with propertytype game-info.  There must be only one @Just@ on any path within a 'GameTree'.
    gameInfo    :: Maybe (GameInfo ruleSet extraGameInfo),
    -- | All properties with propertytype setup or move.
    action      :: Either (Setup stone) (Move move),
    -- | Positional judgments and comments (as opposed to judgments of particular moves).  All properties covered in the \"Node annotation\" section.
    annotation  :: Annotation extraAnnotation,
    -- | How a node should be displayed.  All properties covered in the \"Markup\" and \"Miscellaneous\" sections.
    markup      :: Markup,
    -- | Unspecified properties.
    unknown     :: Map String [[Word8]]
    } deriving (Eq, Ord, Show, Read)

emptyGameNode :: GameNode move stone ruleSet extraGameInfo ()
emptyGameNode = GameNode Nothing (Left emptySetup) emptyAnnotation emptyMarkup Map.empty

type NodeGo            = GameNode MoveGo  Point   RuleSetGo         GameInfoGo              AnnotationGo
type NodeBackgammon    = GameNode ()      ()      RuleSetBackgammon GameInfoBackgammon      ()
type NodeLinesOfAction = GameNode ()      ()      Void              GameInfoLinesOfAction   ()
type NodeHex           = GameNode ()      ()      Void              GameInfoHex             ()
type NodeOcti          = GameNode ()      ()      RuleSetOcti       GameInfoOcti            ()
type NodeOther         = GameNode [Word8] [Word8] Void              ()                      ()
-- }}}
