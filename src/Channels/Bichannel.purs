module Channels.Bichannel 
  ( Bichannel(..)
  , Bisink(..)
  , Bisource(..)
  , Biworkflow(..)
  , awaitDown
  , awaitUp
  , runBiworkflow
  , stack
  , toWorkflow
  , toDownstream
  , toUpstream
  , yieldDown
  , yieldUp
  ) where 

  import Data.Either(Either(..), either)
  import Data.Tuple(Tuple(..))
  import Data.Lazy(Lazy(..), force)
  import Data.Maybe(Maybe(..))
  import Data.Monoid
  import Data.Profunctor(Profunctor, dimap)
  import Control.Apply((*>))
  import Control.Lazy(defer1)
  import Control.Monad.Trans(lift)

  import Channels.Core
  import Channels.Stream(Stream(..), unStream)

  -- | A bidirectional channel, which has both upstream and downstream channels
  -- | of communication.
  type Bichannel a a' b b' f r = Channel (Either a b) (Either a' b') f r
  
  -- | A bisource, defined as a bichannel that never emits upstream values or 
  -- | awaits downstream values.
  type Bisource f a' b r = Bichannel Z a' b Z f r

  -- | A bisink, defined as a bichannel that never emits downstream values or 
  -- | awaits upstream values. 
  type Bisink f a b' r = Bichannel a Z b' Z f r

  -- | A biworkflow, which never awaits or emits upstream or downstream values.
  type Biworkflow f r = Bichannel Z Z Z Z f r

  -- | Converts a stream to an upstream bichannel.
  toUpstream :: forall a f r b b'. (Monad f) => Channel b b' f r -> Bichannel a a b b' f r
  toUpstream = wrapEffect <<< foldChannel yieldF awaitF return
    where yieldF b' c q = (yield (Right b') !: void q) *> toUpstream c
          awaitF    h q = await >>= either (\a -> yield (Left a) *> awaitF h q) (\b -> toUpstream (h b))

  -- | Converts a stream to a downstream bichannel.
  toDownstream :: forall b f r a a'. (Monad f) => Channel a a' f r -> Bichannel a a' b b f r
  toDownstream = wrapEffect <<< foldChannel yieldF awaitF return
    where yieldF a' c q = (yield (Left a') !: void q) *> toDownstream c
          awaitF    h q = await >>= either (\a -> toDownstream (h a)) (\b -> yield (Right b) *> awaitF h q)

  -- | Flips the upstream and downstream channels of the bichannel.
  reflect :: forall a a' b b' f r. (Monad f) => Bichannel a a' b b' f r -> Bichannel b b' a a' f r
  reflect c = unStream (dimap (either Right Left) (either Right Left) (Stream c))

  -- | Using the specified terminator, awaits a downstream value and passes 
  -- | through all upstream values.
  awaitDown :: forall a a' b f. (Monad f) => Bichannel a a' b b f a
  awaitDown = await >>= either pure (\x -> yieldUp x *> awaitDown)

  -- | Using the specified terminator, awaits an upstream value and passes 
  -- | through all downstream values.
  awaitUp :: forall a b b' f. (Monad f) => Bichannel a a b b' f b
  awaitUp = await >>= either (\x -> yieldDown x *> awaitUp) pure


  -- | Using the specified terminator, emits a downstream value.
  yieldDown :: forall a a' b b' f. (Applicative f) => a' -> Bichannel a a' b b' f Unit
  yieldDown a' = yield (Left a')

  -- | Using the specified terminator, emits an upstream value.
  yieldUp :: forall a a' b b' f. (Applicative f) => b' -> Bichannel a a' b b' f Unit
  yieldUp b' = yield (Right b')

  -- | Stacks one bichannel on top of another. Note that if one bichannel 
  -- | terminates before the other, the second will be forcibly terminated.
  -- | 
  -- | Laziness is introduced when the two channels pass messages between each
  -- | other. This allows channels to be stacked even when all they do is 
  -- | forever pass each other messages (e.g. stacking a source on a sink).
  stack :: forall a a' a'' b b' b'' f r r'. (Monad f) => Bichannel a a' b' b'' f r -> Bichannel a' a'' b b' f r' -> Bichannel a a'' b b'' f (Tuple (Maybe r) (Maybe r'))
  stack (Stop r1) c2                 = lift (Tuple (Just r1) <$> runTerminator (terminate c2))
  stack c1 (Stop r2)                 = lift (flip Tuple (Just r2) <$> terminateRun c1)
  stack (Yield (Right b'') c1 q1) c2 = 
    Yield (Right b'') (c1 `stack` c2) (defer1 \_ -> lift (Tuple <$> runTerminator q1 <*> terminateRun c2))
  stack c1 (Yield (Left a'') c2 q2)  = 
    Yield (Left  a'') (c1 `stack` c2) (defer1 \_ -> lift (Tuple <$> terminateRun c1 <*> runTerminator q2))
  stack (ChanX fc1) c2               = ChanX (flip stack c2 <$> fc1)
  stack (ChanZ z1) c2                = ChanZ (flip stack c2 <$> z1)
  stack c1 (ChanX fc2)               = ChanX (stack c1 <$> fc2)
  stack c1 (ChanZ z2)                = ChanZ (stack c1 <$> z2)  
  stack (Await f1 q1) (Yield (Right b') c2 q2) = defer1 \_ -> f1 (Right b') `stack` c2
  stack (Yield (Left a') c1 q1) (Await f2 q2)  = defer1 \_ -> c1 `stack` f2 (Left a')

  -- | Converts a biworkflow to a workflow.
  toWorkflow :: forall f r. (Monad f) => Biworkflow f r -> Workflow f r
  toWorkflow c = unStream (dimap unsafeCoerce unsafeCoerce (Stream c))

  -- | Runs a biworkflow.
  runBiworkflow :: forall f r. (Monad f) => Biworkflow f r -> f r
  runBiworkflow = toWorkflow >>> runWorkflow

  -- This function will never be invoked because sinks are prevented by their 
  -- type signature from emitting values and sources are prevented from using them.
  foreign import unsafeCoerce "function unsafeCoerce(a){return a;}" :: forall a b. a -> b 