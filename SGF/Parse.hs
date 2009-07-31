{-# LANGUAGE NoMonomorphismRestriction #-}
module SGF.Parse where

import Control.Monad
import Data.Char
import Data.Encoding

import SGF.Parse.Encodings
import SGF.Parse.Util
import SGF.Parse.Raw

getFormat   :: Translator Integer
getEncoding :: Translator DynEncoding
getHeader   :: Translator Header
getFormat   = consume "FF" >>= maybe (return 1) number
getEncoding = do
    ws <- consumeSingle "CA"
    case maybe [encodingFromString "latin1"] (guessEncoding . head . values) ws of
        [encoding]  -> return encoding
        []          -> dieWithJust UnknownEncoding   ws
        _           -> dieWithJust AmbiguousEncoding ws -- pretty much guaranteed not to happen
getHeader = liftM2 Header getFormat getEncoding

-- not in use yet, should probably go in Util {{{
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
-- }}}
