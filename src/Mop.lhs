Product and Pairing are the work of Dave Laing and his cofun
series on github at https://github.com/dalaing/cofun

Only minor modifications have been performed to conform to the deprecation of
the OverlappingInstances pragma in GHC 7.10

> module Mop (module Export) where

Monad export for convenience and comonad export for necessity.

> import Control.Monad as Export
> import Control.Comonad as Export

Free/FreeT and Cofree/CofreeT exports since they'll wrap our algebras and
coalgebras to create free monadic algebras and cofree comonadic coalgebras.

> import Control.Monad.Trans.Free as Export
> import Control.Comonad.Trans.Cofree as Export

For convenience, we'll export comonad transformers. They tend to be useful
for defining coalgebras.

> import Control.Comonad.Store    as Export
> import Control.Comonad.Env      as Export
> import Control.Comonad.Identity as Export
> import Control.Comonad.Traced   as Export hiding (Sum(..),Product(..))

The definitions of pairing

> import Product as Export
> import Pairing as Export
