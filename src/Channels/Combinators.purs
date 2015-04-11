module Channels.Combinators where

import Channels.Bichannel
import Channels.Core
import Control.Apply ((*>))
import Data.Foldable (Foldable, foldMap)

-- | Yields a bunch of values.
yieldAll :: forall i o f c. (Monad f, Foldable c) => c o -> Channel i o f Unit
yieldAll co = loop (foldMap (\a -> [a]) co)
  where loop [] = return unit
        loop a @ (x : xs) = yield x *> loop xs

-- | Lifts a pure function to a channel.
moore :: forall i o f. (Monad f) => (i -> o) -> Channel i o f Unit
moore f = loop $ (await >>= (f >>> yield)) !: pure unit

-- | Lifts an effectful function to a channel.
moore' :: forall i o f. (Monad f) => (i -> f o) -> Channel i o f Unit
moore' f = loop $ (await >>= (f >>> yield')) !: pure unit
