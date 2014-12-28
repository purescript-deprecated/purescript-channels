module Channels.Stream 
  ( Stream(..)
  , moore
  , moore'
  , unStream
  ) where

  import Data.Monoid(Monoid, mempty) 
  import Data.Profunctor
  import Control.Lazy(defer1)

  import Channels.Core

  -- | A newtype for Channel so we can define semigroupoid, category, 
  -- | and profunctor.
  newtype Stream f r i o = Stream (Channel i o f r)

  unStream :: forall f r i o. Stream f r i o -> Channel i o f r
  unStream (Stream c) = c

  moore :: forall f r i o. (Applicative f, Monoid r) => (i -> o) -> Stream f r i o
  moore f = Stream (loop (await q (f >>> yield q)))
    where q = pure mempty

  moore' :: forall f r i o. (Applicative f, Monoid r) => (i -> f o) -> Stream f r i o
  moore' f = Stream (loop (await q (f >>> yield' q)))
    where q = pure mempty

  instance semigroupoidStream :: (Applicative f, Semigroup r) => Semigroupoid (Stream f r) where
    (<<<) (Stream c1) (Stream c2) = Stream (compose c1 c2)
      where compose c1 (Await f2 q2)    = Await (compose c1 <$> f2) (q2 <> terminate c1)
            compose (Yield o c1 q1) c2  = Yield o (c1 `compose` c2) (terminate c2 <> q1)
            compose c1 (ChanX fc2 q2)   = ChanX (compose c1 <$> fc2) (q2 <> terminate c1)
            compose c1 (ChanZ zc2)      = ChanZ (compose c1 <$> zc2)
            compose (ChanX fc1 q1) c2   = ChanX (flip compose c2 <$> fc1) (terminate c2 <> q1)
            compose (ChanZ zc1) c2      = ChanZ (flip compose c2 <$> zc1)
            compose (Stop r1) c2        = stop' (flip (<>) r1 <$> runEffectable (terminate c2))
            compose c1 (Stop r2)        = stop' ((<>) r2 <$> runEffectable (terminate c1))
            compose (Await f1 _) (Yield o c2 _) = defer1 \_ -> f1 o `compose` c2

  instance categoryStream :: (Applicative f, Monoid r) => Category (Stream f r) where
    id = Stream (loop (await q (yield q)))
      where q = pure mempty

  instance profunctorStream :: (Applicative f) => Profunctor (Stream f r) where
    dimap f g (Stream c) = Stream (dimap' c)
      where dimap' (Yield o c q) = Yield (g o) (dimap' c) q
            dimap' (Await   h q) = Await (f >>> (dimap' <$> h)) q
            dimap' (ChanX   x q) = ChanX (dimap' <$> x) q
            dimap' (ChanZ     z) = ChanZ (dimap' <$> z)
            dimap' (Stop      r) = Stop r

  