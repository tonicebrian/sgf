{-# LANGUAGE FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module SGF.Parse.Encodings (guessEncoding, decodeWordStringExplicit) where

import Control.Exception.Extensible
import Control.Monad.State
import Control.Throws
import Data.Encoding
import Data.Word

{- TODO: a workaround in case encodingFromStringExplicit isn't available by the time you want to publish:
import Test.ChasingBottoms.IsBottom
encodingFromStringExplicit s = if isBottom enc then Nothing else Just enc where enc = encodingFromString s
-}

type MyIHateGHC = MyEither DecodingException (String, [Word8])
newtype MyEither a b = MyEither (Either a b) deriving (Throws a)

instance Monad (MyEither a) where
    return = MyEither . Right
    (MyEither (Right x)) >>= f = f x
    (MyEither (Left  x)) >>= f = MyEither (Left x)

instance ByteSource (StateT [Word8] (MyEither DecodingException)) where
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
guess ws encoding = case runStateT (decode encoding) ws :: MyIHateGHC of
    (MyEither (Right (s, []))) -> encodingFromStringExplicit s == Just encoding
    _ -> False

guessEncoding ws = filter (guess ws) encodings

decodeWordStringExplicit e ws = case runStateT (decode e) ws :: MyIHateGHC of
    (MyEither (Right (s,_))) -> Right s
    (MyEither (Left  ex   )) -> Left ex
