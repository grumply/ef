{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing -fno-warn-missing-methods #-}
module Ef.Bidir
    ( bidir
    , Bidir
    , runBidir
    , bidirectional

    , Producer
    , Producer'
    , producer

    , Consumer
    , Consumer'
    , consumer

    , Channel
    , channel

    , Client
    , Client'

    , Server
    , Server'

    , Bi
    , Effect
    , Effect'
    , bi

    , Sync.X

    , (<\\)
    , (\<\)
    , (~>)
    , (<~)
    , (/>/)
    , (//>)

    , (//<)
    , (/</)
    , (>~)
    , (~<)
    , (\>\)
    , (>\\)

    , (<~<)
    , (~<<)
    , (>>~)
    , (>~>)

    , (<<+)
    , (<+<)
    , (<-<)
    , (>->)
    , (>+>)
    , (+>>)

    , cat
    , for
    ) where

import Ef

import qualified Ef.Sync as Sync

import Control.Applicative
import Control.Monad
import Unsafe.Coerce

import GHC.Exts

type Bidir = Sync.Sync Int

bidir :: (Monad super, '[Bidir] <. traits)
      => Trait (Sync.Sync Int) traits super
bidir = Sync.sync succ 0

runBidir :: ('[Bidir] <: self, Monad super)
         => Effect self super r -> Narrative self super r
runBidir = Sync.runSync

bidirectional
    :: forall self a a' b b' super r.
       ('[Bidir] <: self, Monad super)
    => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
    -> Sync.Synchronized Int a' a b' b self super r
bidirectional = Sync.synchronized

type Effect self super r = Sync.Effect Int self super r

type Producer b self super r = Sync.Producer Int b self super r

producer :: forall self super b r. ('[Bidir] <: self, Monad super)
         => ((b -> Narrative self super ()) -> Narrative self super r)
         -> Producer' b self super r
producer = Sync.producer

type Consumer a self super r = Sync.Consumer Int a self super r

consumer :: forall self super a r. ('[Bidir] <: self, Monad super)
         => (Narrative self super a -> Narrative self super r)
         -> Consumer' a self super r
consumer = Sync.consumer

type Channel a b self super r = Sync.Channel Int a b self super r

channel :: forall self super a b r. ('[Bidir] <: self, Monad super)
     => (Narrative self super a -> (b -> Narrative self super ()) -> Narrative self super r)
     -> Channel a b self super r
channel = Sync.channel

type Client a' a self super r = Sync.Client Int a' a self super r

type Server b' b self super r = Sync.Server Int b' b self super r

runSync = Sync.runSync

type Bi a' a b' b self super r = Sync.Synchronized Int a' a b' b self super r

bi :: forall self a a' b b' super r. ('[Bidir] <: self, Monad super)
        => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
        -> Bi a' a b' b self super r
bi = Sync.synchronized

type Effect' self super r = Sync.Effect' Int self super r

type Producer' b self super r = Sync.Producer' Int b self super r

type Consumer' a self super r = Sync.Consumer' Int a self super r

type Server' b' b self super r = Sync.Server' Int b' b self super r

type Client' a' a self super r = Sync.Client' Int a' a self super r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: ('[Bidir] <: self, Monad super) => Channel a a self super r
cat = Sync.cat

infixl 3 //>
(//>) :: ('[Bidir] <: self, Monad super)
      => Bi x' x b' b self super a'
      -> (b -> Bi x' x c' c self super b')
      -> Bi x' x c' c self super a'
p0 //> fb = p0 Sync.//> fb

for :: ('[Bidir] <: self, Monad super)
    => Bi x' x b' b self super a'
    -> (b -> Bi x' x c' c self super b')
    -> Bi x' x c' c self super a'
for = Sync.for

infixr 3 <\\
(<\\) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi x' x c' c self super b')
      -> Bi x' x b' b self super a'
      -> Bi x' x c' c self super a'
f <\\ p = f Sync.<\\ p

infixl 4 \<\
(\<\) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi x' x c' c self super b')
      -> (a -> Bi x' x b' b self super a')
      -> a
      -> Bi x' x c' c self super a'
p1 \<\ p2 = p1 Sync.\<\ p2

infixr 4 ~>
(~>) :: ('[Bidir] <: self, Monad super)
     => (a -> Bi x' x b' b self super a')
     -> (b -> Bi x' x c' c self super b')
     -> a
     -> Bi x' x c' c self super a'
(~>) = (Sync.~>)

infixl 4 <~
(<~) :: ('[Bidir] <: self, Monad super)
     => (b -> Bi x' x c' c self super b')
     -> (a -> Bi x' x b' b self super a')
     -> a
     -> Bi x' x c' c self super a'
g <~ f = g Sync.<~ f

infixr 4 />/
(/>/) :: ('[Bidir] <: self, Monad super)
      => (a -> Bi x' x b' b self super a')
      -> (b -> Bi x' x c' c self super b')
      -> a
      -> Bi x' x c' c self super a'
(fa />/ fb) a = (fa Sync./>/ fb) a

--------------------------------------------------------------------------------
-- Request; substitute awaits

infixr 4 >\\
(>\\) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a y' y self super b)
      -> Bi b' b y' y self super c
      -> Bi a' a y' y self super c
fb' >\\ p0 = fb' Sync.>\\ p0

infixr 5 /</
(/</) :: ('[Bidir] <: self, Monad super)
      => (c' -> Bi b' b x' x self super c)
      -> (b' -> Bi a' a x' x self super b)
      -> c'
      -> Bi a' a x' x self super c
p1 /</ p2 = p1 Sync./</ p2

infixr 5 >~
(>~) :: ('[Bidir] <: self, Monad super)
     => Bi a' a y' y self super b
     -> Bi () b y' y self super c
     -> Bi a' a y' y self super c
p1 >~ p2 = p1 Sync.>~ p2

infixl 5 ~<
(~<) :: ('[Bidir] <: self, Monad super)
     => Bi () b y' y self super c
     -> Bi a' a y' y self super b
     -> Bi a' a y' y self super c
p2 ~< p1 = p2 Sync.~< p1

infixl 5 \>\
(\>\) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a y' y self super b)
      -> (c' -> Bi b' b y' y self super c)
      -> c'
      -> Bi a' a y' y self super c
(fb' \>\ fc') c' = (fb' Sync.\>\ fc') c'

infixl 4 //<
(//<) :: ('[Bidir] <: self, Monad super)
      => Bi b' b y' y self super c
      -> (b' -> Bi a' a y' y self super b)
      -> Bi a' a y' y self super c
p //< f = p Sync.//< f

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

infixl 7 >>~
(>>~)
    :: forall self a' a b' b c' c super r.
       (Monad super, '[Bidir] <: self)
    => Bi a' a b' b self super r
    -> (b -> Bi b' b c' c self super r)
    -> Bi a' a c' c self super r
p0 >>~ fb0 = p0 Sync.>>~ fb0

infixl 8 <~<
(<~<) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi b' b c' c self super r)
      -> (a -> Bi a' a b' b self super r)
      -> a
      -> Bi a' a c' c self super r
p1 <~< p2 = p1 Sync.<~< p2

infixr 8 >~>
(>~>) :: ('[Bidir] <: self, Monad super)
      => (_a -> Bi a' a b' b self super r)
      -> (b -> Bi b' b c' c self super r)
      -> _a
      -> Bi a' a c' c self super r
(fa >~> fb) a = (fa Sync.>~> fb) a

infixr 7 ~<<
(~<<) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi b' b c' c self super r)
      -> Bi a' a b' b self super r
      -> Bi a' a c' c self super r
k ~<< p = k Sync.~<< p

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

infixr 6 +>>
(+>>) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a b' b self super r)
      ->        Bi b' b c' c self super r
      ->        Bi a' a c' c self super r
fb' +>> p0 = fb' Sync.+>> p0

infixl 7 >->
(>->) :: ('[Bidir] <: self, Monad super)
      => Bi a' a () b self super r
      -> Bi () b c' c self super r
      -> Bi a' a c' c self super r
p1 >-> p2 = p1 Sync.>-> p2

infixr 7 <-<
(<-<) :: ('[Bidir] <: self, Monad super)
      => Bi () b c' c self super r
      -> Bi a' a () b self super r
      -> Bi a' a c' c self super r
p2 <-< p1 = p2 Sync.<-< p1

infixr 7 <+<
(<+<) :: ('[Bidir] <: self, Monad super)
      => (c' -> Bi b' b c' c self super r)
      -> (b' -> Bi a' a b' b self super r)
      -> c'
      -> Bi a' a c' c self super r
p1 <+< p2 = p1 Sync.<+< p2

infixl 7 >+>
(>+>) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a b' b self super r)
      -> (_c' -> Bi b' b c' c self super r)
      -> _c'
      -> Bi a' a c' c self super r
(fb' >+> fc') c' = (fb' Sync.>+> fc') c'

infixl 6 <<+
(<<+) :: ('[Bidir] <: self, Monad super)
      => Bi b' b c' c self super r
      -> (b' -> Bi a' a b' b self super r)
      -> Bi a' a c' c self super r
p <<+ fb = p Sync.<<+ fb