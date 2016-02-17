module Channels.Combinators where

  import Prelude (class Monad, Unit, unit, pure, (>>>), (>>=), ($), bind)

  import Data.Foldable (class Foldable, foldl)

  import Channels.Core (Channel, yield', await, (!:), loop, yield)

  -- | Yields a bunch of values.
  yieldAll :: forall i o f c. (Monad f, Foldable c) => c o -> Channel i o f Unit
  yieldAll = foldl (\next x -> do yield x
                                  next)
                   (pure unit)

  -- | Lifts a pure function to a channel.
  moore :: forall i o f. (Monad f) => (i -> o) -> Channel i o f Unit
  moore f = loop $ (await >>= (f >>> yield)) !: pure unit

  -- | Lifts an effectful function to a channel.
  moore' :: forall i o f. (Monad f) => (i -> f o) -> Channel i o f Unit
  moore' f = loop $ (await >>= (f >>> yield')) !: pure unit
