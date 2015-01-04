module Channels.Core
  ( Channel(..)
  , Terminator()
  , Sink(..)
  , Source(..)
  , Workflow(..)
  , Z()
  , await
  , compose
  , finalizer
  , loop
  , loopForever
  , moore
  , moore'
  , nonTerminator
  , runTerminator
  , runWorkflow
  , terminate
  , terminateRun
  , terminator
  , wrapEffect
  , yield
  , yield'
  ) where 

  import Data.Array((!!))
  import Data.Foldable(Foldable, foldl, foldr, foldMap)
  import Data.Traversable(Traversable, traverse, sequence)
  import Data.Maybe(Maybe(..), maybe)
  import Data.Monoid(Monoid, mempty)
  import Data.Tuple(Tuple(..))
  import Data.Lazy(Lazy(..), force, defer)
  import Control.Lazy(Lazy1, defer1)
  import Control.Bind
  import Control.Monad.Trans(MonadTrans, lift)
  import Control.Apply

  foreign import data Z :: *

  -- | A channel terminator, which may terminate with a value, or refuse to 
  -- | terminate. Termination can be accompanied by effects, including 
  -- | laziness or `f` effects.
  data Terminator f a = TerP a | TerE | TerX (f (Terminator f a)) | TerZ (Lazy (Terminator f a))

  -- | An event-driven channel of communication with a well-defined lifecycle.
  -- | 
  -- | Channels may yield output values, await input values, execute effects, 
  -- | defer computation of a channel, and voluntarily terminate with a final
  -- | result value `r`.
  -- |
  -- | All channels may be forcefully terminated to produce an `f (Maybe r)`.
  data Channel i o f r
    = Yield o (Channel i o f r) (Terminator f r)
    | Await (i -> Channel i o f r) (Terminator f r)
    | ChanX (f (Channel i o f r)) (Terminator f r)
    | ChanZ (Lazy (Channel i o f r))
    | Stop r  

  -- | A source of values, which awaits nothing.
  type Source o f r = Channel Z o f r

  -- | A sink of values, which emits nothing.
  type Sink i f r = Channel i Z f r

  -- | A workflow consists of a source composed with a sink.
  type Workflow f r = Channel Z Z f r

  foreign import unsafeZ "val unsafeZ = undefined;" :: Z

  nonTerminator :: forall f a. Terminator f a
  nonTerminator = TerE

  -- | Lifts a pure function to a channel.
  moore :: forall i o f. (Monad f) => (i -> o) -> Channel i o f Unit
  moore f = loop $ terminator (pure unit) (await >>= (f >>> yield))

  -- | Lifts an effectful function to a channel.
  moore' :: forall i o f. (Monad f) => (i -> f o) -> Channel i o f Unit
  moore' f = loop $ terminator (pure unit) (await >>= (f >>> yield'))

  -- | Pipes the output of one channel to the input of another.
  compose :: forall a b c f r. (Monad f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r
  compose c1 (Await f2 q2)    = Await (compose c1 <$> f2) (q2 <> terminate c1)
  compose (Yield o c1 q1) c2  = Yield o (c1 `compose` c2) (terminate c2 <> q1)
  compose c1 (ChanX fc2 q2)   = ChanX (compose c1 <$> fc2) (q2 <> terminate c1)
  compose c1 (ChanZ zc2)      = ChanZ (compose c1 <$> zc2)
  compose (ChanX fc1 q1) c2   = ChanX (flip compose c2 <$> fc1) (terminate c2 <> q1)
  compose (ChanZ zc1) c2      = ChanZ (flip compose c2 <$> zc1)
  compose (Stop r1) c2        = lift (maybe r1 (flip (<>) r1) <$> terminateRun c2)
  compose c1 (Stop r2)        = lift (maybe r2 (     (<>) r2) <$> terminateRun c1)
  compose (Await f1 _) (Yield o c2 _) = defer1 \_ -> f1 o `compose` c2


  -- | Runs an terminator to produce an `f (Maybe a)`.
  runTerminator :: forall f a. (Monad f) => Terminator f a -> f (Maybe a)
  runTerminator (TerP  a) = pure (Just a)
  runTerminator TerE      = pure (Nothing)
  runTerminator (TerX fa) = fa >>= runTerminator
  runTerminator (TerZ ef) = runTerminator (force ef)

  -- | Runs a workflow to completion. TODO: stack overflow.
  runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r
  runWorkflow w = loop w
    where loop (Yield _ c _) = loop c
          loop (Await   f _) = loop (f unsafeZ)
          loop (ChanX   x _) = x >>= loop
          loop (ChanZ     z) = loop (force z)
          loop (Stop      r) = pure r

  -- | Returns a looping channel that restarts when it reaches the end.
  loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r
  loop c0 = loop' c0
    where loop' (Yield o c q) = Yield o (loop' c) q
          loop' (Await   f q) = Await (loop' <$> f) q
          loop' (ChanX   x q) = ChanX (loop' <$> x) q
          loop' (ChanZ     z) = ChanZ (loop' <$> z)
          loop' (Stop      _) = loop' c0


  -- | Returns a looping channel that will refuse to terminate.
  loopForever :: forall i o f r r'. (Monad f) => Channel i o f r -> Channel i o f r'
  loopForever c0 = loop' c0
    where loop' (Yield o c q) = Yield o (loop' c) (q *> nonTerminator)
          loop' (Await   f q) = Await (loop' <$> f) (q *> nonTerminator)
          loop' (ChanX   x q) = ChanX (loop' <$> x) (q *> nonTerminator)
          loop' (ChanZ     z) = ChanZ (loop' <$> z)
          loop' (Stop      r) = loop' c0
  
  -- | Awaits a value and monadically returns it.
  await :: forall i o f. (Applicative f) => Channel i o f i
  await = Await pure nonTerminator

  -- | Yields a value.
  yield :: forall i o f. (Applicative f) => o -> Channel i o f Unit
  yield o = Yield o stopUnit terminatorUnit

  -- | Using the specified terminator, yields an effectful value.
  yield' :: forall i o f. (Monad f) => f o -> Channel i o f Unit
  yield' fo = wrapEffect (yield <$> fo)

  -- | Wraps an effect into the channel.
  wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r
  wrapEffect fc = ChanX fc (lift fc >>= terminate)

  -- | Forcibly terminates a channel and returns a terminator.
  terminate :: forall i o f r. (Applicative f) => Channel i o f r -> Terminator f r
  terminate (Yield _ _ q) = q
  terminate (Await   _ q) = q
  terminate (ChanX   _ q) = q
  terminate (ChanZ     l) = defer1 \_ -> (terminate (force l))
  terminate (Stop      r) = pure r

  -- | Terminates a channel and runs the terminator.
  terminateRun :: forall i o f r. (Monad f) => Channel i o f r -> f (Maybe r)
  terminateRun = terminate >>> runTerminator

  -- | Replaces the value that the channel will produce if forcibly terminated.
  -- | Preserves any effects associated with the old terminator.
  terminator :: forall i o f r. (Applicative f) => Terminator f r -> Channel i o f r -> Channel i o f r
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
  finalizer :: forall i o f r x. (Monad f) => f x -> Channel i o f r -> Channel i o f r
  finalizer x = loop
    where
      x' = TerX (TerP <$> x)

      loop (Yield o c q) = Yield o (loop c) (x' *> q)
      loop (Await   f q) = Await (loop <$> f) (x' *> q)
      loop (ChanX   x q) = ChanX (loop <$> x) (x' *> q)
      loop (ChanZ     z) = ChanZ (loop <$> z)
      loop (Stop      r) = lift x *> Stop r

  stopUnit :: forall i o f. Channel i o f Unit
  stopUnit = Stop unit

  terminatorUnit :: forall f. Terminator f Unit
  terminatorUnit = TerP unit

  -- Terminator instances
  instance showTerminator :: (Functor f, Show (f String), Show a) => Show (Terminator f a) where 
    show (TerP a) = "TerP (" ++ show a ++ ")"
    show TerE     = "TerE"
    show (TerX x) = "TerX (" ++ show (show <$> x) ++ ")"
    show (TerZ z) = "TerZ (" ++ show z ++ ")"

  instance lazy1Terminator :: Lazy1 (Terminator f) where
    defer1 l = TerZ (defer l)

  instance functorTerminator :: (Functor f) => Functor (Terminator f) where 
    (<$>) f (TerP a) = TerP (f a)
    (<$>) f TerE     = TerE
    (<$>) f (TerX x) = TerX ((<$>) f <$> x)
    (<$>) f (TerZ z) = TerZ ((<$>) f <$> z)

  -- TODO: Implement apply and bind more efficiently!
  instance applyTerminator :: (Applicative f) => Apply (Terminator f) where
    (<*>) (TerP f) (TerP x) = TerP (f x)
    (<*>) TerE            _ = TerE
    (<*>) _            TerE = TerE
    (<*>) (TerX f)        x = TerX (flip (<*>) x <$> f)
    (<*>) (TerZ f)        x = TerZ (flip (<*>) x <$> f)
    (<*>) f        (TerX x) = TerX ((<*>) f <$> x)
    (<*>) f        (TerZ x) = TerZ ((<*>) f <$> x)

  instance applicativeTerminator :: (Applicative f) => Applicative (Terminator f) where
    pure a = TerP a

  instance bindTerminator :: (Monad f) => Bind (Terminator f) where
    (>>=) (TerP a) f = f a
    (>>=) TerE     f = TerE
    (>>=) (TerX x) f = TerX (flip (>>=) f <$> x)
    (>>=) (TerZ z) f = defer1 \_ -> force z >>= f

  instance monadTerminator :: (Monad f) => Monad (Terminator f)

  instance monadTransTerminator :: MonadTrans Terminator where 
    lift fa = TerX (TerP <$> fa)

  instance semigroupTerminator :: (Monad f, Semigroup a) => Semigroup (Terminator f a) where
    (<>) x y = defer1 \_ -> TerX (maybe TerE TerP <$> ((<>) <$> runTerminator x <*> runTerminator y))

  instance monoidTerminator :: (Monad f, Semigroup a) => Monoid (Terminator f a) where 
    mempty = TerE

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
    lift fr = wrapEffect (pure <$> fr)