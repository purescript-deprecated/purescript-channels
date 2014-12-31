module Channels.Core
  ( Channel(..)
  , Effectable()
  , Sink(..)
  , Source(..)
  , Workflow(..)
  , Z()
  , await
  , compose
  , effect
  , finalizer
  , loop
  , moore
  , moore'
  , runEffectable
  , runWorkflow
  , stop
  , stop'
  , terminate
  , terminator
  , wrapEffect
  , yield
  , yield'
  ) where 

  import Data.Array((!!))
  import Data.Foldable(Foldable, foldl, foldr, foldMap)
  import Data.Traversable(Traversable, traverse, sequence)
  import Data.Maybe(maybe)
  import Data.Monoid(Monoid, mempty)
  import Data.Tuple(Tuple(..))
  import Data.Lazy(Lazy(..), force, defer)
  import Control.Lazy(Lazy1, defer1)
  import Control.Bind
  import Control.Monad.Trans(MonadTrans, lift)
  import Control.Apply

  foreign import data Z :: *

  -- | A value whose optionally lazy computation may or may not require an effect `f`.
  -- | This exists mainly for performance reasons, as always associating all values
  -- | with lazily computed effects adds several layers of indirection.
  data Effectable f a = EffP a | EffX (f a) | EffZ (Lazy (Effectable f a))

  -- | An event-driven channel of communication with a well-defined lifecycle.
  -- | 
  -- | Channels may yield output values, await input values, execute effects, 
  -- | defer computation of a channel, and voluntarily terminate with a final
  -- | result value `r`.
  -- |
  -- | All channels may be forcefully terminated to produce an `f r`.
  data Channel i o f r
    = Yield o (Channel i o f r) (Effectable f r)
    | Await (i -> Channel i o f r) (Effectable f r)
    | ChanX (f (Channel i o f r)) (Effectable f r)
    | ChanZ (Lazy (Channel i o f r))
    | Stop r  

  -- | A source of values, which awaits nothing.
  type Source o f r = Channel Z o f r

  -- | A sink of values, which emits nothing.
  type Sink i f r = Channel i Z f r

  -- | A workflow consists of a source composed with a sink.
  type Workflow f r = Channel Z Z f r

  foreign import unsafeZ "val unsafeZ = undefined;" :: Z

  -- | Lifts a pure function to a channel.
  moore :: forall f r i o. (Applicative f, Monoid r) => (i -> o) -> Channel i o f r
  moore f = loop (await q (f >>> yield q))
    where q = pure mempty

  -- | Lifts an effectful function to a channel.
  moore' :: forall f r i o. (Applicative f, Monoid r) => (i -> f o) -> Channel i o f r
  moore' f = loop (await q (f >>> yield' q))
    where q = pure mempty

  arraySource :: forall f o. (Monad f) => [o] -> Source o f Number
  arraySource a = loop 0
    where loop i = maybe (stop i) (cont i) (a !! i)
          cont i e = do yield (pure i) e
                        loop (i + 1)

  arraySink :: forall f i. (Monad f) => Sink i f [i]
  arraySink = loop [] 
    where loop a = await (pure a) \i -> loop (a <> [i]) -- TODO: List then convert to Array!!!

  -- | Pipes the output of one channel to the input of another.
  compose :: forall a b c f r. (Applicative f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r
  compose c1 (Await f2 q2)    = Await (compose c1 <$> f2) (q2 <> terminate c1)
  compose (Yield o c1 q1) c2  = Yield o (c1 `compose` c2) (terminate c2 <> q1)
  compose c1 (ChanX fc2 q2)   = ChanX (compose c1 <$> fc2) (q2 <> terminate c1)
  compose c1 (ChanZ zc2)      = ChanZ (compose c1 <$> zc2)
  compose (ChanX fc1 q1) c2   = ChanX (flip compose c2 <$> fc1) (terminate c2 <> q1)
  compose (ChanZ zc1) c2      = ChanZ (flip compose c2 <$> zc1)
  compose (Stop r1) c2        = stop' (flip (<>) r1 <$> runEffectable (terminate c2))
  compose c1 (Stop r2)        = stop' ((<>) r2 <$> runEffectable (terminate c1))
  compose (Await f1 _) (Yield o c2 _) = defer1 \_ -> f1 o `compose` c2

  -- | Runs an effectable to produce an `f a`. The requirement for 
  -- | `Applicative` could be loosened if the return value were Either a (f a).
  runEffectable :: forall f a. (Applicative f) => Effectable f a -> f a
  runEffectable (EffP  a) = pure a
  runEffectable (EffX fa) = fa
  runEffectable (EffZ ef) = runEffectable (force ef)

  -- | Runs a workflow to completion. TODO: stack overflow.
  runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r
  runWorkflow w = loop w
    where loop (Yield _ c _) = loop c
          loop (Await   f _) = loop (f unsafeZ)
          loop (ChanX   x _) = x >>= loop
          loop (ChanZ     z) = loop (force z)
          loop (Stop      r) = pure r

  -- | Returns a new channel that will restart when it reaches the end. 
  -- | Although the channel will never voluntarily terminate, it may still be
  -- | forcibly terminated.
  loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r
  loop c0 = loop' c0
    where loop' (Yield o c q) = Yield o (loop' c) q
          loop' (Await   f q) = Await (loop' <$> f) q
          loop' (ChanX   x q) = ChanX (loop' <$> x) q
          loop' (ChanZ     z) = ChanZ (loop' <$> z)
          loop' (Stop      _) = c0

  -- | Using the specified terminator, awaits a value.
  await :: forall i o f r. Effectable f r -> (i -> Channel i o f r) -> Channel i o f r
  await q f = Await f q

  -- | Using the specified terminator, yields a value.
  yield :: forall i o f r. (Applicative f) => Effectable f r -> o -> Channel i o f r
  yield fr o = Yield o (stop' (runEffectable fr)) fr

  -- | Using the specified terminator, yields an effectful value.
  yield' :: forall i o f r. (Applicative f) => Effectable f r -> f o -> Channel i o f r
  yield' fr fo = ChanX (yield fr <$> fo) fr

  -- | Produces a channel that stops the channel with the pure value `r`.
  stop :: forall i o f r. r -> Channel i o f r
  stop r = Stop r

  -- | Produces a channel that stops the channel with the effectful value `f r`.
  stop' :: forall i o f r. (Functor f) => f r -> Channel i o f r
  stop' fr = (ChanX (stop <$> fr)) (EffX fr)

  effect :: forall i o f r x. (Applicative f) => Effectable f r -> f x -> Channel i o f r
  effect q fx = stop' (fx *> runEffectable q)

  -- | Wraps an effect into the channel.
  wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r
  wrapEffect fc = ChanX fc (lift (fc >>= (terminate >>> runEffectable)))

  -- | Forcibly terminates a channel (unless the channel has already 
  -- | voluntarily terminated).
  terminate :: forall i o f r. (Applicative f) => Channel i o f r -> Effectable f r
  terminate (Yield _ _ q) = q
  terminate (Await   _ q) = q
  terminate (ChanX   _ q) = q
  terminate (ChanZ     l) = EffZ (defer \_ -> terminate (force l))
  terminate (Stop      r) = EffP r

  -- | Replaces the value that the channel will produce if forcibly terminated.
  -- | Preserves any effects associated with the old terminator.
  terminator :: forall i o f r. (Applicative f) => Effectable f r -> Channel i o f r -> Channel i o f r
  terminator q2 = loop
    where
      loop (Yield o c q1) = Yield o (loop c) (q1 *> q2)
      loop (Await   f q1) = Await (loop <$> f) (q1 *> q2)
      loop (ChanX   x q1) = ChanX (loop <$> x) (q1 *> q2)
      loop (ChanZ      z) = ChanZ (loop <$> z)
      loop (Stop       r) = Stop r

  -- | Attaches the specified finalizer to the channel. The finalizer will be
  -- | called when the channel is forcibly terminated or when it voluntarily 
  -- | terminates (but just once).
  finalizer :: forall i o f r x. (Applicative f) => f x -> Channel i o f r -> Channel i o f r
  finalizer x = loop
    where
      x' = EffX x

      loop (Yield o c q) = Yield o (loop c) (x' *> q)
      loop (Await   f q) = Await (loop <$> f) (x' *> q)
      loop (ChanX   x q) = ChanX (loop <$> x) (x' *> q)
      loop (ChanZ     z) = ChanZ (loop <$> z)
      loop (Stop      r) = stop' x *> Stop r

  -- Effectable instances
  instance showEffectable :: (Show (f a), Show a) => Show (Effectable f a) where 
    show (EffP a) = "EffP (" ++ show a ++ ")"
    show (EffX x) = "EffX (" ++ show x ++ ")"
    show (EffZ z) = "EffZ (" ++ show z ++ ")"

  instance lazy1Effectable :: Lazy1 (Effectable f) where
    defer1 l = EffZ (defer l)

  instance functorEffectable :: (Functor f) => Functor (Effectable f) where 
    (<$>) f (EffP a) = EffP (f a)
    (<$>) f (EffX x) = EffX (f <$> x)
    (<$>) f (EffZ z) = EffZ ((<$>) f <$> z)

  -- TODO: Implement apply and bind more efficiently!
  instance applyEffectable :: (Applicative f) => Apply (Effectable f) where
    (<*>) f x = defer1 \_ -> EffX (runEffectable f <*> runEffectable x)

  instance applicativeEffectable :: (Applicative f) => Applicative (Effectable f) where
    pure a = EffP a

  instance bindEffectable :: (Monad f) => Bind (Effectable f) where
    (>>=) fa f = defer1 \_ -> EffX (runEffectable fa >>= (runEffectable <$> f))

  instance monadEffectable :: (Monad f) => Monad (Effectable f)

  instance monadTransEffectable :: MonadTrans Effectable where 
    lift = EffX

  instance semigroupEffectable :: (Applicative f, Semigroup a) => Semigroup (Effectable f a) where
    (<>) x y = defer1 \_ -> (EffX ((<>) <$> runEffectable x <*> runEffectable y))

  instance monoidEffectable :: (Applicative f, Monoid a) => Monoid (Effectable f a) where 
    mempty = EffP mempty

  instance foldableEffectable :: (Applicative f, Foldable f) => Foldable (Effectable f) where
    foldr f b fa = foldr f b (runEffectable fa)
    foldl f b fa = foldl f b (runEffectable fa)
    foldMap f fa = foldMap f (runEffectable fa)

  instance traversableEffectable :: (Applicative f, Traversable f) => Traversable (Effectable f) where
    traverse f ta = EffX <$> traverse f (runEffectable ta)

    sequence tma = EffX <$> sequence (runEffectable tma)

  -- Channel instances
  instance lazy1Channel :: Lazy1 (Channel i o f) where
    defer1 l = ChanZ (defer l)

  instance functorChannel :: (Functor f) => Functor (Channel i o f) where
    (<$>) f (Yield o c q) = Yield o (f <$> c) (f <$> q)
    (<$>) f (Await   g q) = Await ((<$>) f <$> g) (f <$> q)
    (<$>) f (ChanX   x q) = ChanX ((<$>) f <$> x) (f <$> q)
    (<$>) f (ChanZ     z) = ChanZ ((<$>) f <$> z)
    (<$>) f (Stop      r) = Stop (f r)

  instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r) where
    (<>) (Yield o c q) w = Yield o (c <> w) q
    (<>) (Await   f q) w = Await (flip (<>) w <$> f) q
    (<>) (ChanX   x q) w = ChanX (flip (<>) w <$> x) q
    (<>) (ChanZ     z) w = ChanZ (flip (<>) w <$> z)
    (<>) (Stop      r) w = (<>) r <$> w

  instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r) where
    mempty = Stop mempty 

  instance applyChannel :: (Applicative f) => Apply (Channel i o f) where
    (<*>) (Yield o c q) w = Yield o (c <*> w) (q <*> terminate w)
    (<*>) (Await   g q) w = Await (flip (<*>) w <$> g) (q <*> terminate w)
    (<*>) (ChanX   x q) w = ChanX (flip (<*>) w <$> x) (q <*> terminate w)
    (<*>) (ChanZ     z) w = ChanZ (flip (<*>) w <$> z)
    (<*>) v @ (Stop f) (Yield o c q) = Yield o (v <*> c) (pure f <*> q)
    (<*>) v @ (Stop f) (Await  g q)  = Await ((<*>) v <$> g) (pure f <*> q)
    (<*>) v @ (Stop f) (ChanX  x q)  = ChanX ((<*>) v <$> x) (pure f <*> q)
    (<*>) v @ (Stop f) (ChanZ    z)  = ChanZ ((<*>) v <$> z)
    (<*>) v @ (Stop f) (Stop     x)  = Stop (f x)

  instance applicativeChannel :: (Applicative f) => Applicative (Channel i o f) where
    pure r = Stop r

  instance bindChannel :: (Monad f) => Bind (Channel i o f) where
    (>>=) (Yield o c q) f = Yield o (c >>= f) (q >>= (terminate <$> f))
    (>>=) (Await   g q) f = Await (flip (>>=) f <$> g) (q >>= (terminate <$> f))
    (>>=) (ChanX   x q) f = ChanX (flip (>>=) f <$> x) (q >>= (terminate <$> f))
    (>>=) (ChanZ     z) f = ChanZ (flip (>>=) f <$> z)
    (>>=) (Stop      r) f = f r

  instance monadChannel :: (Monad f) => Monad (Channel i o f)

  instance monadTransChannel :: MonadTrans (Channel i o) where 
    lift = stop'