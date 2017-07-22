module Ef.Writer
  ( Writer, writerFromp, writerFrom, writerp, writer
  , pattern TellP, pattern Tell
  , notedp, noted
  , tellp, tell
  ) where

import Ef

import Data.Monoid

data Writer p r k where
  Writer
    :: { _writer_proxy :: Proxy p
       , written :: !r
       , write :: r -> k
       } -> Writer p r k


  Tell_
    :: { tell_writer_proxy :: Proxy p
       , value :: r
       , result :: k
       } -> Writer p r k

  deriving Functor

pattern TellP p n = Tell_ p n (Return ())

pattern Tell n <- Tell_ (Proxy :: Proxy ()) n (Return ()) where
  Tell n = Tell_ (Proxy :: Proxy ()) n (Return ())

instance Delta (Writer p r) (Writer p r) where
  delta eval Writer {..} Tell_ {..} = eval (write value) result

writerFromp :: forall c p w ts. (Monad c, ts <. '[Writer p w])
            => Proxy p -> w -> (w -> w -> w) -> Writer p w (Action ts c)
writerFromp p w0 f =
  Writer p w0 $ \new o ->
    let Module (Writer {..} :: Writer p w (Action ts c)) o = o
    in return $ Module (Writer { written = f written new, .. }) o

writerFrom :: (Monad c, ts <. '[Writer () w])
           => w -> (w -> w -> w) -> Writer () w (Action ts c)
writerFrom = writerFromp unit

writerp :: forall c p ts w. (Monad c, ts <. '[Writer p w], Monoid w)
        => Proxy p -> w -> Writer p w (Action ts c)
writerp p w = writerFromp p w (<>)

writer :: forall c ts w. (Monad c, ts <. '[Writer () w], Monoid w)
       => w -> Writer () w (Action ts c)
writer w = writerp unit w

notedp :: forall p c ts w. (Monad c, ts <. '[Writer p w])
       => Proxy p -> Object ts c -> w
notedp _ (Module (Writer {..} :: Writer p w (Action ts c)) o) = written

noted :: (Monad c, ts <. '[Writer () w]) => Object ts c -> w
noted = notedp unit

tellp :: forall p w ms c. (Monad c, ms <: '[Writer p w])
      => Proxy p -> w -> Ef ms c ()
tellp p = Send . TellP p

tell :: (Monad c, ms <: '[Writer () w])
     => w -> Ef ms c ()
tell = Send . Tell
