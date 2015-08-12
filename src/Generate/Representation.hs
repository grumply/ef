{-# LANGUAGE DeriveDataTypeable #-}
module Generate.Representation where

import Generate.Monad

import Data.Data

data SymbolSet = SymbolSet
  { symbolSetName            :: Name
  , symbolSetType            :: Decl
  , symbolSetComponents      :: [Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data InstructionSet = InstructionSet
  { instructionSetName       :: Name
  , instructionSetType       :: Decl
  , instructionSetComponents :: [Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Instructions = Instructions
  { instructions             :: [Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Interpreters = Interpreters
  { interpreters             :: [Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Pairings = Pairings
  { pairings                 :: [(Decl,Decl,Decl)]
  } deriving (Show,Read,Eq,Data,Typeable)
