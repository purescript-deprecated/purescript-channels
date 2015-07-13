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

  import Prelude hiding (compose)

  import Data.Either(Either(..), either)
  import Data.Tuple(Tuple(..))
  import Data.Lazy(Lazy(..), force)
  import Data.Maybe(Maybe(..))
  import Data.Monoid
  import Data.Profunctor(Profunctor, dimap)
  import Control.Apply((*>))
  import Control.Lazy(defer)
  import Control.Monad.Trans(lift)

  import Channels.Core
  import Channels.Stream(Stream(..), unStream)
  
  import Unsafe.Coerce (unsafeCoerce)

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
    where yieldF b' c q = yield (Right b') !: void q *> toUpstream c
          awaitF    h q = await >>= either (\a -> yield (Left a) *> awaitF h q) (\b -> toUpstream (h b))

  -- | Converts a stream to a downstream bichannel.
  toDownstream :: forall b f r a a'. (Monad f) => Channel a a' f r -> Bichannel a a' b b f r
  toDownstream = wrapEffect <<< foldChannel yieldF awaitF return
    where yieldF a' c q = yield (Left a') !: void q *> toDownstream c
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
  -- | Laziness is introduced when channels communicate to each other as a way
  -- | of avoiding stack overflows. Of course, for long-running computations,
  -- | the base monad must also be trampolined to avoid overflowing the stack.
  stack :: forall a a' a'' b b' b'' f r r'. (Monad f) => Bichannel a a' b' b'' f r -> Bichannel a' a'' b b' f r' -> Bichannel a a'' b b'' f (Tuple (Maybe r) (Maybe r'))
  stack c1 c2 = wrapEffect (foldChannel yieldF1 awaitF1 stopF1 c1)
    where
      yieldF1 e1 c1 q1  = either (\a' ->
                                  let yieldF2 e2 c2 q2 = either (\a'' -> yield (Left a'') !: void q2 *>
                                                                        (yield (Left a') *> c1) `stack` c2)
                                                                (\b'  -> feed' (Right b') c1 `stack` feed' (Left a') c2) e2
                                      awaitF2    f2  _ = defer \_ -> c1 `stack` f2 (Left a')
                                      stopF2        r2 = lift (flip Tuple (Just r2) <$> runTerminator q1)
                                  in wrapEffect (foldChannel yieldF2 awaitF2 stopF2 c2))
                                 (\b'' -> yield (Right b'') !: void q1 *> c1 `stack` c2) e1

      awaitF1    f1 q1  = let c1 = await >>= f1 -- q1

                              yieldF2 e2 c2 q2 = either (\a'' -> yield (Left a'') !: void q2 *> c1 `stack` c2)
                                                        (\b'  -> defer \_ -> f1 (Right b') `stack` c2) e2
                              awaitF2    f2 q2 = await >>= either (\a -> f1 (Left a) `stack` c2)
                                                                  (\b -> c1 `stack` f2 (Right b))
                              stopF2        r2 = lift (flip Tuple (Just r2) <$> runTerminator q1)
                          in wrapEffect (foldChannel yieldF2 awaitF2 stopF2 c2)

      stopF1        r1  = lift (Tuple (Just r1) <$> terminateRun c2)

  -- | Converts a biworkflow to a workflow.
  toWorkflow :: forall f r. (Monad f) => Biworkflow f r -> Workflow f r
  toWorkflow c = unStream (dimap unsafeCoerce unsafeCoerce (Stream c))

  -- | Runs a biworkflow.
  runBiworkflow :: forall f r. (Monad f) => Biworkflow f r -> f r
  runBiworkflow = toWorkflow >>> runWorkflow
