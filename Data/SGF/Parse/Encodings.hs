{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-noncanonical-monad-instances #-}

module Data.SGF.Parse.Encodings (guessEncoding, decodeWordStringExplicit) where

import Control.Applicative (Applicative (..))
import Control.Exception.Extensible
import Control.Monad (ap, liftM)
import Control.Monad.State
import Control.Throws
import Data.Encoding
import Data.Word

type MyIHateGHC = MyEither DecodingException (String, [Word8])

newtype MyEither a b = MyEither (Either a b) deriving (Throws a)

instance Functor (MyEither a) where
  fmap = liftM

instance Applicative (MyEither a) where
  pure = return
  (<*>) = ap

instance Monad (MyEither a) where
  return = MyEither . Right
  (MyEither (Right x)) >>= f = f x
  (MyEither (Left x)) >>= f = MyEither (Left x)

instance ByteSource (StateT [Word8] (MyEither DecodingException)) where
  sourceEmpty = gets null
  fetchWord8 = do
    s <- get
    case s of
      [] -> throwException UnexpectedEnd
      c : cs -> put cs >> return c
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

-- |
-- Try decoding the given word string with each of the known-good encodings to
-- see if the decoded name names the encoding used to decode.  It should be
-- impossible for this to return a list with more than one guess.
guessEncoding :: [Word8] -> [DynEncoding]
guessEncoding ws = filter (guess ws) encodings

-- |
-- A simple wrapper around the encoding package's 'decode' function.
decodeWordStringExplicit :: Encoding e => e -> [Word8] -> Either DecodingException String
decodeWordStringExplicit e ws = case runStateT (decode e) ws :: MyIHateGHC of
  (MyEither (Right (s, _))) -> Right s
  (MyEither (Left ex)) -> Left ex
