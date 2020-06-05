module Text.Parsing.Combinators.Validation
  ( accept
  , reject
  , fail
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Maybe (Maybe(..))

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input. 
-- | Does not consume the remaining parse input.
accept :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
accept = \x p -> do
  y <- C.optionMaybe $ C.lookAhead p
  case y of
    (Just z)  -> pure z
    (Nothing) -> lift x

-- | Accepts a default value `x` in in the case that the parser `p` fails when applied to the remaining parse input. 
-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Does not consume the remaining parse input.
reject :: forall a m b. Monad m => m b -> ParserT a m b -> ParserT a m b
reject = \x p -> do
  y <- C.optionMaybe $ C.lookAhead p
  case y of
    (Just _)  -> P.fail $ "Validation failure: rejected successful parse result."
    (Nothing) -> lift x 

-- | Fails if the result of applying the parser `p` to the remaining parse input is successful.
-- | Does not consume the remaining parse input.
fail :: forall a m b. Monad m => ParserT a m b -> ParserT a m Unit
fail = \p -> reject (pure unit) (p *> pure unit)
