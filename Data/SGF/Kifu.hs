{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.SGF.Kifu (
    goban,
    goBoard
) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB

-- For testing within GHCI
-- :main -w 640 -h 480 -o /tmp/test.svg

-- Basic point
ten = mconcat [ hrule 1 ,
                vrule 1 ]

-- Go Boards
goban = goBoard 19

goBoard size = grid # clipBy border <> wood
    where
        center = let c = (size-1) `div` 2
                 in (fromIntegral c, fromIntegral (-c))
        border = translate (r2 center) (square (fromIntegral (size-1))) 
        wood = border # fc (sRGB24 220 179 99)
        grid = vcat $ replicate size row
        row = hcat $ replicate size ten

-- Stones
wStone = circle 0.5 # fc white
bStone = circle 0.5 # fc black

-- Joseki
joseki = bold . font "arial" $ mconcat [
            translate (r2 (2,-16)) wStone
            , translate (r2 (3,-15)) bStone
            , translate (r2 (2,-15)) (text "1" # scale 0.9 <> wStone)
            , translate (r2 (3,-14)) (text "2" # fc white # scale 0.9 <> bStone)
            , translate (r2 (4,-17)) (text "3" # scale 0.9 <> wStone)
            , translate (r2 (6,-15)) ((text "4" # fc white # scale 0.9) <> bStone)
            ]

-- Helper function
main = defaultMain $ mconcat [joseki,goban]
