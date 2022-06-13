{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.DefinitionContext where

import Data.Validity
import GHC.Generics (Generic)
import Sparep.Data.DeckName
import Sparep.Data.Instructions

-- | Context that is gathered when resolving a deck
--
-- This does not contain anything that will be hashed, and everything inside the 'definitionContextUnit' should be hashed.
data DefinitionContext a = DefinitionContext
  { definitionContextUnit :: !a,
    definitionContextDeckName :: !(Maybe DeckName),
    definitionContextInstructions :: !(Maybe Instructions)
  }
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (DefinitionContext a)
