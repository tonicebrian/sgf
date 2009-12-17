{-| The Smart Games Format (SGF) is a rich way of storing records of games,
along with metadata about the games.  The format is arranged in a tree
structure; to a first approximation, each node in the tree corresponds to a
move in one of the games being recorded.

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
forests.
-}
module Data.SGF (
    module Data.SGF.Types,
    module Data.SGF.Parse
) where

-- It should be noted that the SGF specification talks not only about the file
-- format, but also about the behavior of applications that read and write SGF
-- files.  This library cannot guarantee that applications conform to that
-- specification.

import Data.SGF.Types
import Data.SGF.Parse (collection)

-- TODO:
-- * support the rest of the SGF format ;-)
-- * clean up exports and imports as much as possible
-- * documentation for usage
-- * commenting/documentation for hacking
