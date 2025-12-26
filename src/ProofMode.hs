module ProofMode
  ( ProofMode(..)
  , defaultProofMode
  ) where

data ProofMode = Sound | Unsafe
  deriving (Show, Eq)

defaultProofMode :: ProofMode
defaultProofMode = Sound
