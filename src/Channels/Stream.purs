module Channels.Stream
  ( Stream(..)
  , unStream
  ) where

  import Prelude (class Monad, class Category, class Semigroup, class Semigroupoid, pure, (<$>), (>>>), (>>=), void)

  import Data.Profunctor (class Profunctor)

  import Control.Apply ((*>))

  import Channels.Core (Channel, foldChannel, wrapEffect, await, yield, (!:), loopForever, compose)

  -- | A newtype for Channel so we can define semigroupoid, category,
  -- | and profunctor.
  newtype Stream f r i o = Stream (Channel i o f r)

  unStream :: forall f r i o. Stream f r i o -> Channel i o f r
  unStream (Stream c) = c

  instance semigroupoidStream :: (Monad f, Semigroup r) => Semigroupoid (Stream f r) where
    compose (Stream c1) (Stream c2) = Stream (compose c1 c2)

  instance categoryStream :: (Monad f, Semigroup r) => Category (Stream f r) where
    id = Stream (loopForever (await >>= yield))

  instance profunctorStream :: (Monad f) => Profunctor (Stream f r) where
    dimap f g (Stream c) = Stream (loop c)
      where yieldF o c q = (yield (g o) !: void q) *> loop c
            awaitF   h q = await >>= (f >>> (loop <$> h))
            loop       c = wrapEffect (foldChannel yieldF awaitF pure c)
