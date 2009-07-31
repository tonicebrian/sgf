{-# LANGUAGE FlexibleInstances #-}
module SGF.Parse.Encodings (guessEncoding) where

import Control.Exception.Extensible
import Control.Monad.State
import Control.Throws
import Data.Encoding
import Data.Word

instance ByteSource (StateT [Word8] (Either DecodingException)) where
    sourceEmpty = gets null
    fetchWord8  = do
        s <- get
        case s of
            []      -> throwException UnexpectedEnd
            c:cs    -> put cs >> return c
    fetchAhead m = do
        s <- get
        v <- m
        put s
        return v

-- some ones that we know satisfy our invariant (see SGF.Parse.Raw)
encodings = map encodingFromString ["latin1", "utf-8", "ascii"]
guess ws encoding = case runStateT (decode encoding) ws of
    Right (s, []) -> encodingFromStringExplicit s == Just encoding
    _             -> False

guessEncoding ws = filter (guess ws) encodings
