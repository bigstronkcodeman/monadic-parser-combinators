{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Parsing.ParserTypes (
  Parser(..),
  Pos(..),
  Result(..)
) where

import Text.Printf (printf)
import Data.Text.Lazy as TL (Text, unpack, empty)
import Data.List (intercalate)
import Control.Monad (MonadPlus(..), (>=>))
import Control.Applicative (Alternative(..))


data Pos = Pos !Int !Int deriving (Eq)

instance Show Pos where
  show (Pos row col) = printf "(%d,%d)" row col

type ErrorContext = [String]
data Result r = Failure Text !Pos ErrorContext String
              | Partial (Text -> Result r)
              | Finished Text r

instance (Show r) => Show (Result r) where
  show (Failure inp pos ctx s) = printf "Failure {rest = \"%s\", pos = %s, ctx = [%s], err = \"%s\"}" (unpack inp) (show pos) (intercalate "," ctx) s
  show (Partial _)             = "Partial ?"
  show (Finished inp r)        = printf "Finished {rest = \"%s\", r = %s}" (unpack inp) (show r)

instance Functor Result where
  fmap _ (Failure inp pos ctx e) = Failure inp pos ctx e
  fmap f (Partial k)             = Partial (fmap f . k)
  fmap f (Finished inp r)        = Finished inp (f r)

instance Applicative Result where
  pure = Finished TL.empty

  (Failure inp pos ctx e) <*> _       = Failure inp pos ctx e
  _ <*> (Failure inp pos ctx e)       = Failure inp pos ctx e
  (Finished _ f) <*> (Finished inp x) = Finished inp (f x)
  (Finished _ f) <*> (Partial k)      = Partial (fmap f . k)
  (Partial f) <*> r                   = Partial (\inp -> f inp <*> r)

instance Monad Result where
  return = pure

  (Failure inp pos ctx e) >>= _ = Failure inp pos ctx e
  (Finished _ x)          >>= f = f x
  (Partial g)             >>= f = Partial (g >=> f)



type Failure r   = Text -> Pos -> ErrorContext -> String -> Result r
type Success a r = Text -> Pos -> a            -> Result r

newtype Parser a = Parser { runParser :: forall r. Text -> Pos -> Failure r -> Success a r -> Result r }

instance Functor Parser where
  fmap f p = Parser $ \inp !pos kf ks ->
    let ks' inp' !pos' x = ks inp' pos' (f x)
    in runParser p inp pos kf ks'

instance Applicative Parser where
  pure x = Parser $ \inp !pos _ ks ->
    ks inp pos x

  pf <*> px = Parser $ \inp !pos kf ks ->
    let ksf inp' !pos' f = let ksx inp'' !pos'' x = ks inp'' pos'' (f x)
                           in runParser px inp' pos' kf ksx
    in runParser pf inp pos kf ksf

instance Alternative Parser where
  empty = fail ""

  p <|> q = Parser $ \inp !pos kf ks ->
    let kf' _ _ _ _ = runParser q inp pos kf ks
    in runParser p inp pos kf' ks

instance Monad Parser where
  return = pure

  px >>= f = Parser $ \inp !pos kf ks ->
    let ksx inp' !pos' x = runParser (f x) inp' pos' kf ks
    in runParser px inp pos kf ksx

instance MonadFail Parser where
  fail s = Parser $ \inp !pos kf _ ->
    kf inp pos [] s

instance MonadPlus Parser where
  mzero = fail "mzero"

  mplus = (<|>)

instance (Semigroup a) => Semigroup (Parser a) where
  p <> q = (<>) <$> p <*> q

instance (Monoid a) => Monoid (Parser a) where
  mempty = pure mempty

  mappend = (<>)
