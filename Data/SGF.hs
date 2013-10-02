-- boilerplate {{{
{-|
This is a parser for the go\/igo\/weiqi\/baduk fragment of the SGF format.
Encodings latin-1, utf-8, and ascii are supported, and the parser strives to be
robust to minor errors, especially those made by the most common SGF editors.
There are plans to support other games and pretty-printing in future releases.
-}
module Data.SGF (
    module Data.SGF.Types,
    module Data.SGF.Parse,
    module Data.SGF.Kifu,
    module Data.Word,
    module Data.Tree,
    module Text.ParserCombinators.Parsec
    -- * Overview of SGF
    -- $sgf

    -- * Overview of the library
    -- $library

    -- * Example usage
    -- $example
) where

import Data.SGF.Types
import Data.SGF.Parse (collection)
import Data.SGF.Kifu
import Data.Word
import Data.Tree
import Text.ParserCombinators.Parsec (runParser)

-- TODO:
-- * support parsing from ByteString, then take the "unpack" out of "parse" below
-- * support the rest of the SGF format ;-)
-- * clean up exports and imports as much as possible
-- }}}
-- sgf {{{
{- $sgf
The Smart Games Format (SGF) is a rich way of storing records of games, along
with metadata about the games.  The format is arranged in a tree structure; to
a first approximation, each node in the tree corresponds to a move in one of
the games being recorded.

There are a few ways this first approximation needs to be corrected:

1. It is actually a non-empty forest: there may be several trees, each of which
is totally independent.  In particular, different trees may be recording not
only different games, but even different game /types/: the file may contain
records of both go and chess games, for example.  Each root has games of only a
single type, but there may be many actual games of that type.  There is no
restriction that different roots use different game types.  (Luckily, most
real-world files have have a single root and record a single game.)

2. A tree may encode variations within a single game.  When there are many
branches in a tree, there may or may not be an indication of which are separate
games and which are variations.

3. Each node may additionally record metadata.  This metadata may be about the
game (for example, the names of the players), about the position (for example,
that the whole board favors the black player), or about the move (for example,
that the move is good for the black player).

4. Some nodes may record a modification of the game board rather than a move in
the game.  This is used especially for collections of problems, to record only
parts of a game (i.e. to set up the initial position), and to handle other
strange situations.

This module provides a way to parse files in the SGF format and traverse game
forests.  In addition to specifying a file format, the SGF specification talks
about the behavior of applications that read and write SGF files.  Wherever
possible, these behaviors are enforced or encouraged via types.  It is
considered a bug if a required behavior is impossible, if an unrecommended
behavior is easy, or if a prohibited behavior is allowed and the documentation
does not have warnings in appropriate places.
-}
-- }}}
-- library {{{
{- $library
The two basic starting points are 'collection' and 'Collection'.

The former is a Parsec-3 parser for consuming entire files in the SGF format.
It additionally reports some warnings for the most common and easily-corrected
errors in files, but these can generally be safely ignored.

The latter is a largish type which is intended to be inhabitable by all and
only the valid SGF trees.  There are a few places where we sacrifice this
property in the name of convenience; wherever possible, the documentation
states any restrictions on values that are not reflected by the type system.
-}
-- }}}
-- example {{{
{- $example
In this section, we develop a short program to print, in human-readable form,
the moves of the main line of a game of go.  We will assume that we are given
the contents of an SGF file on @stdin@, that the file only records one game on
a standard size board, that the file doesn't do anything fancy (like change the
move number in the middle of the game), and that any node without a valid move
is the end of the game.  (This saves us a lot of error-checking that would just
obscure the idea.)

First, some boring stuff.

> import Data.SGF
> import Data.ByteString  (ByteString, getContents, unpack)
> import Data.Tree
> import Data.List hiding ((!!))
> import Prelude   hiding ((!!), getContents)
>
> (!!) = genericIndex

Now we'll extract the 'TreeGo' representing the game.  As suggested in the
overview section, we'll use the 'collection' parser.

> grabTree :: [Word8] -> TreeGo
> grabTree s = case runParser collection () "stdin" s of
>     Right ([Game { tree = TreeGo t }], _) -> t

We're already assuming huge swathes of things about the input; this is a
horribly partial function.  But let's continue.  The next step is to extract
the moves from the main line.  The main line is specified to be the first child
at each possible junction, so this is not too tricky: the 'levels' function of
'Data.Tree' (together with a 'transpose') will flatten the game tree in just
the right way that the first element will be the main line.

The one thing we will watch out for is that games of go often begin with one
setup node to give the handicap stones, which we want to skip.  Actually, for
simplicity, we'll just skip every single setup node.  Setup nodes are signaled
by via the 'action' field of 'GameNode's: if this field is a 'Left', then it is
a setup node, and otherwise it is a move node.

> grabMoves :: TreeGo -> [MoveGo]
> grabMoves n = [move | Right Move { move = Just (color, move) } <- mainLine]
>     where mainLine = map action . head . transpose . levels $ n

Let's wrap these functions up:

> parse :: ByteString -> [MoveGo]
> parse = grabMoves . grabTree . unpack

Now that we have the moves, we need a way to show them.  In go, it's standard
practice to give coordinates as a single upper-case letter followed by a
number.  To avoid confusion, the letter \'I\' is skipped as a coordinate, since
it looks much like a one in some fonts.

> coordinates :: [Char]
> coordinates = delete 'I' ['A'..'Z']
>
> showPoint :: Point -> String
> showPoint (x, y) = coordinates !! x : show (19 - y)

We'll pad moves out to four spaces, and show them in pairs.

> pad :: String -> String
> pad s = s ++ replicate (4 - length s) ' '
>
> showMove :: MoveGo -> String
> showMove Pass = "pass"
> showMove (Play p) = pad (showPoint p)
>
> showMoves :: [MoveGo] -> String
> showMoves = unlines . showMoves' 1 where
>     showMoves' n [ ]       = []
>     showMoves' n [m]       = unwords [show n ++ ".", showMove m]              : []
>     showMoves' n (m:m':ms) = unwords [show n ++ ".", showMove m, showMove m'] : showMoves' (n+2) ms

Finally, we just need some plumbing:

> main :: IO ()
> main = putStr . showMoves . parse =<< getContents

And there we have it: a complete program to parse an SGF file, extract the moves, and print them!
-}
-- }}}
