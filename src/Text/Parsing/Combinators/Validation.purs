module Text.Parsing.Combinators.Validation
  ( and
  , or
  , not
  , input
  , output
  , success
  , failure
  , left
  , right
  , just
  , nothing
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Except.Trans as E

import Control.Monad.State.Class as S
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C

-- | Accepts a default value of `Nothing` in the case that the parser `p` fails when applied to the remaining parse input.
-- | Does not consume the remaiing parse input.
maybe :: forall a m b. Monad m => ParserT a m b -> ParserT a m (Maybe b)
maybe = C.optionMaybe <<< C.lookAhead

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input.
-- | Does not consume the remaining parse input.
succeedWith :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
succeedWith = \x p -> do
  y <- maybe p
  case y of
    (Just z)  -> pure z
    (Nothing) -> lift x

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input.
-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Does not consume the remaining parse input if `p` is successful.
failWith :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
failWith = \x p -> do
  y <- maybe p
  case y of
    (Just _)  -> P.fail $ "Validation failure: rejected successful parse result."
    (Nothing) -> lift x

-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Does not consume the remaining parse input.
not :: forall a m b. Monad m => ParserT a m b -> ParserT a m Unit
not = \p -> failWith (pure unit) (p *> pure unit)

-- | Attempts to validate the remaining parse input with a parser `q`, or a parser `p` if `q` fails.
-- | Fails if the validation of the remaining parse input with parsers `p` or `q` is unsuccessful.
-- | Does not consume the remaining parse input.
or :: forall a m b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Either b c)
or = \p q -> do
  x <- maybe q
  case x of
    (Nothing) -> do
      y <- maybe p
      case y of
        (Nothing) -> P.fail $ "Validation failure: both validation attempts were unsuccessful."
        (Just z)  -> pure $ Left z
    (Just z) -> pure $ Right z

-- | Attempts to validate the remaining parse input with a parser `p` and a parser `q`.
-- | Fails if the validation of the remaining parse input with parsers `p` and `q` is unsuccessful.
-- | Does not consume the remaining parse input.
and :: forall a m b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Tuple b c)
and = \p q -> do
  w <- maybe p
  x <- maybe q
  case (Tuple w x) of
    (Tuple (Nothing) (Just _))  -> P.fail $ "Validation failure: first validation attempt was unsuccessful."
    (Tuple (Just _)  (Nothing)) -> P.fail $ "Validation failure: second validation attempt was unsuccessful."
    (Tuple (Nothing) (Nothing)) -> P.fail $ "Validation failure: both validation attempts were unsuccessful."
    (Tuple (Just y)  (Just z))  -> pure $ Tuple y z

-- | Fails if the parsed result is left.
-- | Does not consume the remaining parse input.
left :: forall a m b c. Monad m => (Either b c) -> ParserT a m b
left (Right x) = P.fail $ "Validation failure: expected left result."
left (Left x)  = pure x

-- | Fails if the parsed result is right.
-- | Does not consume the remaining parse input.
right :: forall a m b c. Monad m => (Either b c) -> ParserT a m c
right (Left x)  = P.fail $ "Validation failure: expected right result."
right (Right x) = pure x

-- | Lifts the remaining parse input for validation purposes.
-- | Does not consume the remaining parse input.
getInput :: forall a m. Monad m => ParserT a m a
getInput = S.gets \(P.ParseState x _ _) -> x

-- | Runs a parser `p` against the remaining parse input for validation purposes.
-- | Does not consume the remaining parse input.
input :: forall a m b. Monad m => ParserT a m b -> ParserT a m b
input = \p -> do
  w <- getInput
  x <- pure $ P.runParserT w p
  y <- lift x
  z <- right y
  pure z

-- | Runs a parser `p` against the remaining parse input, and `q` against the result if successful.
-- | Does not consume the remaining parse input.
output :: forall a m b c. Monad m => ParserT a m b -> ParserT b m c -> ParserT a m c
output = \p q -> do
  w <- input p
  x <- pure $ P.runParserT w q
  y <- lift x
  z <- right y
  pure z

-- | Fails if the parsed result throws an exception when run.
-- | Dones not consume remaining parse input
success :: forall a m b c. Monad m => ExceptT a m b -> ParserT c m b
success = \w -> do
  x <- lift $ E.runExceptT w
  y <- right x
  pure y

-- | Fails if the parsed result throws is successful when run.
-- | Dones not consume remaining parse input
failure :: forall a m b c. Monad m => ExceptT a m b -> ParserT c m a
failure = \w -> do
  x <- lift $ E.runExceptT w
  y <- left x
  pure y

-- | Fails if the parsed result is nothing.
-- | Does not consume the remaining parse input.
just :: forall a m b. Monad m => (Maybe b) -> ParserT a m b
just (Nothing)  = P.fail $ "Validation failure: did not expect nothing result."
just (Just x)   = pure x

-- | Fails if the parsed result is not nothing.
-- | Does not consume the remaining parse input.
nothing :: forall a m b. Monad m => (Maybe b) -> ParserT a m Unit
nothing (Just _)  = P.fail $ "Validation failure: expected nothing result."
nothing (Nothing) = pure unit
