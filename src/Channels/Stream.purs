module Channels.Stream 
  ( Stream(..)
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

  instance semigroupoidStream :: (Monad f, Semigroup r) => Semigroupoid (Stream f r) where
    (<<<) (Stream c1) (Stream c2) = Stream (compose c1 c2)

  instance categoryStream :: (Monad f, Semigroup r) => Category (Stream f r) where
    id = Stream (loopForever (await >>= yield))

  instance profunctorStream :: (Monad f) => Profunctor (Stream f r) where
    dimap f g (Stream c) = Stream (dimap' c)
      where dimap' (Yield o c q) = Yield (g o) (dimap' c) q
            dimap' (Await   h q) = Await (f >>> (dimap' <$> h)) q
            dimap' (ChanX   x q) = ChanX (dimap' <$> x) q
            dimap' (ChanZ     z) = ChanZ (dimap' <$> z)
            dimap' (Stop      r) = Stop r

  