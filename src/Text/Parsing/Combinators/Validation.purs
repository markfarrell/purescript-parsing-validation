module Text.Parsing.Combinators.Validation
  ( succeed
  , fail
  , or
  , and
  , left
  , right
  , input
  , output
  ) where

import Prelude

import Control.Monad.State.Class (gets)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input.
-- | Consumes the remaining parse input.
accept :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
accept = \x p -> do
  y <- C.optionMaybe $ p
  case y of
    (Just z)  -> pure z
    (Nothing) -> lift x

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input.
-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Consumes the remaining parse input if `p` is successful.
reject :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
reject = \x p -> do
  y <- C.optionMaybe p
  case y of
    (Just _)  -> P.fail $ "Validation failure: rejected successful parse result."
    (Nothing) -> lift x

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input.
-- | Does not consume the remaining parse input.
succeed :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
succeed = \x p -> accept x $ C.lookAhead p

-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Does not consume the remaining parse input.
fail :: forall a m b. Monad m => ParserT a m b -> ParserT a m Unit
fail = \p -> reject (pure unit) (C.lookAhead p *> pure unit)

-- | Attempts to validate the remaining parse input with a parser `q`, or a parser `p` if `q` fails.
-- | Fails if the validation of the remaining parse input with parsers `p` or `q` is unsuccessful.
-- | Does not consume the remaining parse input.
or :: forall a m b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Either b c)
or = \p q -> do
  x <- C.optionMaybe $ C.lookAhead q
  case x of
    (Nothing) -> do
      y <- C.optionMaybe $ C.lookAhead p
      case y of
        (Nothing) -> P.fail $ "Validation failure: both validation attempts were unsuccessful."
        (Just z)  -> pure $ Left z
    (Just z) -> pure $ Right z

-- | Attempts to validate the remaining parse input with a parser `p` and a parser `q`.
-- | Fails if the validation of the remaining parse input with parsers `p` and `q` is unsuccessful.
-- | Does not consume the remaining parse input.
and :: forall a m b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Tuple b c)
and = \p q -> do
  w <- C.optionMaybe $ C.lookAhead p
  x <- C.optionMaybe $ C.lookAhead q
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
getInput = gets \(P.ParseState x _ _) -> x 

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
