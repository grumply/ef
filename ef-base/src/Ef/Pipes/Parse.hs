module Ef.Pipes.Parse where

import Ef
import Ef.State
import Ef.Pipes

import Prelude hiding (span, splitAt)

data Parser a m = forall x. Parser (Producer a m x)

draw :: forall self super a.
        (Monad super, '[State (Parser a super)] <: self)
     => Narrative self super (Maybe a)
draw = do
  Parser p <- get
  x <- super (next p)
  case x of
    Left r -> do
      put (Parser $ return r)
      return Nothing
    Right (a,p') -> do
      put (Parser p')
      return (Just a)
