{-# LANGUAGE NoMonomorphismRestriction #-}
module SGF.Parse where

import Data.Char
import Data.Encoding
import Data.List
import Data.Word

import SGF.Parse.Raw
import SGF.Types

data ParseHeader = ParseHeader {
    format   :: Integer,
    encoding :: DynEncoding
}

emptyHeader = ParseHeader { format = 1, encoding = encodingFromString "latin1" }

errorDangling = error "Impossible: dangling escape character"
newline empty withNewline withoutNewline xs = case xs of
    '\r':'\n':xs -> withNewline xs
    '\n':'\r':xs -> withNewline xs
    '\r':     xs -> withNewline xs
    '\n':     xs -> withNewline xs
    x   :     xs -> withoutNewline x xs
    []           -> empty

descapeChar soft x xs      = (if isSpace x then ' ' else x) : descapeText soft xs
descapeText soft ('\\':xs) = newline errorDangling (descapeText soft)           (descapeChar soft) xs
descapeText soft xs        = newline ""            ((soft:) . descapeText soft) (descapeChar soft) xs

descapeSimple = descapeText ' '
descape       = descapeText '\n'

duplicates = concatMap (take 1 . drop 1) . group . sort
