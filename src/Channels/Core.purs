module Channels.Core where 
  import Data.Foldable(Foldable, foldl, foldr, foldMap)
  import Data.Traversable(Traversable, traverse, sequence)
  import Data.Monoid(Monoid, mempty)
  import Data.Either(Either(..))
  import Data.Tuple(Tuple(..))
  import Data.Lazy(Lazy(..), force, defer)
  import Control.Lazy(Lazy1, defer1)
  import Control.Bind
  import Control.Monad.Trans(MonadTrans, lift)
  import Control.Apply

  -- | A value whose optionally lazy computation may or may not require an effect `f`.
  -- | This exists mainly for performance reasons, as always associating all values
  -- | with lazily computed effects adds several layers of indirection.
  data Effectable f a = EffP a | EffX (f a) | EffZ (Lazy (Effectable f a))

  -- | A bidirectional, event-driven channel of communication. 
  -- | 
  -- | Channels have an upstream component, which transforms `b` to `b'`, and
  -- | a downstream component, which transforms `a` to `a'`. 
  -- | 
  -- | Channels may emit values upstream or downstream, await upstream / 
  -- | downstream values, execute an effect, defer computation of a channel, 
  -- | or voluntarily terminate with a result value `r`.
  -- |
  -- | All channels may be forcefully terminated to produce an `f r`.
  -- | 
  data Channel a a' b b' f r
    = Emit (Either a' b') (Channel a a' b b' f r) (Effectable f r)
    | Await (Either a b -> Channel a a' b b' f r) (Effectable f r)
    | ChanX (f (Channel a a' b b' f r)) (Effectable f r)
    | ChanZ (Lazy (Channel a a' b b' f r))
    | Stop r

  newtype Upstream a f r b b' = Upstream (Channel a a b b' f r)

  newtype Downstream b f r a a' = Downstream (Channel a a' b b f r)

  type UniChannel a b f r = Channel a a b b f r

  runEffectable :: forall f a. (Applicative f) => Effectable f a -> f a
  runEffectable (EffP a) = pure a
  runEffectable (EffX fa)   = fa
  runEffectable (EffZ ef)   = runEffectable (force ef)

  unUpstream :: forall a f r b b'. Upstream a f r b b' -> Channel a a b b' f r
  unUpstream (Upstream c) = c

  unDownstream :: forall b f r a a'. Downstream b f r a a' -> Channel a a' b b f r
  unDownstream (Downstream c) = c

  -- | Using the specified terminator, awaits an upstream or downstream value.
  await :: forall a a' b b' f r. Effectable f r -> (Either a b -> Channel a a' b b' f r) -> Channel a a' b b' f r
  await q f = Await f q

  -- | Using the specified terminator, awaits a downstream value and passes 
  -- | through all upstream values.
  awaitDown :: forall a a' b f r. (Applicative f) => Effectable f r -> (a -> Channel a a' b b f r) -> Channel a a' b b f r
  awaitDown q f = await q g
    where 
      g (Left a)  = f a
      g (Right b) = emitUp q b

  -- | Using the specified terminator, awaits an upstream value and passes 
  -- | through all downstream values.
  awaitUp :: forall a b b' f r. (Applicative f) => Effectable f r -> (b -> Channel a a b b' f r) -> Channel a a b b' f r
  awaitUp q f = await q g
    where 
      g (Left a)  = emitDown q a 
      g (Right b) = f b

  -- | Using the specified terminator, emits an upstream or downstream value.
  emit :: forall a a' b b' f r. (Applicative f) => Effectable f r -> Either a' b' -> Channel a a' b b' f r
  emit fr e = Emit e (yield' (runEffectable fr)) fr

  -- | Using the specified terminator, emits a downstream value.
  emitDown :: forall a a' b b' f r. (Applicative f) => Effectable f r -> a' -> Channel a a' b b' f r
  emitDown q a' = emit q (Left a')

  -- | Using the specified terminator, emits an upstream value.
  emitUp :: forall a a' b b' f r. (Applicative f) => Effectable f r -> b' -> Channel a a' b b' f r
  emitUp q b' = emit q (Right b')

  -- | Produces a channel that monadically returns the pure value `r`.
  yield :: forall a a' b b' f r. r -> Channel a a' b b' f r
  yield r = Stop r

  -- | Produces a channel that monadically returns the effectful value `f r`.
  yield' :: forall a a' b b' f r. (Functor f) => f r -> Channel a a' b b' f r
  yield' fr = (ChanX (yield <$> fr)) (EffX fr)

  -- | Forcibly terminates a channel (unless the channel has already 
  -- | voluntarily terminated).
  terminate :: forall a a' b b' f r. (Applicative f) => Channel a a' b b' f r -> Effectable f r
  terminate (Emit _ _ q) = q
  terminate (Await  _ q) = q
  terminate (ChanX  _ q) = q
  terminate (ChanZ    l) = EffZ (defer \_ -> terminate (force l))
  terminate (Stop     r) = EffP r

  -- | Stacks one channel on top of another. Note that if one channel 
  -- | terminates before the other, the second will be forcibly terminated.
  -- | 
  -- | Laziness is introduced when the two channels pass messages between each
  -- | other. This allows channels to be stacked even when all they do is 
  -- | forever pass each other messages.
  stack :: forall a a' a'' b b' b'' f r r'. (Applicative f) => Channel a a' b' b'' f r -> Channel a' a'' b b' f r' -> Channel a a'' b b'' f (Tuple r r')
  stack (Emit (Right b'') c1 q1) c2 = Emit (Right b'') (c1 `stack` c2)  (defer1 \_ -> (Tuple <$> q1 <*> terminate c2))
  stack c1 (Emit (Left a'') c2 q2)  = Emit (Left a'') (c1 `stack` c2)   (defer1 \_ -> (Tuple <$> terminate c1 <*> q2))
  stack (ChanX fc1 q1) c2           = ChanX (flip stack c2 <$> fc1)     (defer1 \_ -> (Tuple <$> q1 <*> terminate c2))
  stack c1 (ChanX fc2 q2)           = ChanX (stack c1 <$> fc2)          (defer1 \_ -> (Tuple <$> terminate c1 <*> q2))
  stack (ChanZ z1) c2               = ChanZ (flip stack c2 <$> z1)
  stack c1 (ChanZ z2)               = ChanZ (stack c1 <$> z2)
  stack (Stop r1) c2                = yield' (Tuple r1 <$> runEffectable (terminate c2))
  stack c1 (Stop r2)                = yield' (flip Tuple r2 <$> runEffectable (terminate c1))
  stack (Await f1 q1) (Emit (Right b') c2 q2) = defer1 \_ -> f1 (Right b') `stack` c2
  stack (Emit (Left a') c1 q1) (Await f2 q2)  = defer1 \_ -> c1 `stack` f2 (Left a')

  -- | Replaces the value that the channel will produce if forcibly terminated.
  terminator :: forall a a' b b' f r. (Applicative f) => Effectable f r -> Channel a a' b b' f r -> Channel a a' b b' f r
  terminator q = loop
    where
      loop (Emit e c _) = Emit e (loop c) q
      loop (Await  f _) = Await (loop <$> f) q
      loop (ChanX  x _) = ChanX (loop <$> x) q
      loop (ChanZ    z) = ChanZ (loop <$> z)
      loop (Stop     r) = Stop r

  -- | Attaches the specified finalizer to the channel. The finalizer will be
  -- | called when the channel is forcibly terminated or when it voluntarily 
  -- | terminates (but just once).
  finalizer :: forall a a' b b' f r x. (Applicative f) => f x -> Channel a a' b b' f r -> Channel a a' b b' f r
  finalizer x = loop
    where
      x' = EffX x

      loop (Emit e c q) = Emit e (loop c) (x' *> q)
      loop (Await  f q) = Await (loop <$> f) (x' *> q)
      loop (ChanX  x q) = ChanX (loop <$> x) (x' *> q)
      loop (ChanZ    z) = ChanZ (loop <$> z)
      loop (Stop     r) = yield' x *> Stop r

  instance showEffectable :: (Show (f a), Show a) => Show (Effectable f a) where 
    show (EffP a) = "EffP (" ++ show a ++ ")"
    show (EffX    x) = "EffX (" ++ show x ++ ")"
    show (EffZ    z) = "EffZ (" ++ show z ++ ")"

  instance lazy1Effectable :: Lazy1 (Effectable f) where
    defer1 l = EffZ (defer l)

  instance functorEffectable :: (Functor f) => Functor (Effectable f) where 
    (<$>) f (EffP a) = EffP (f a)
    (<$>) f (EffX    x) = EffX (f <$> x)
    (<$>) f (EffZ    z) = EffZ ((<$>) f <$> z)

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



  instance lazy1Channel :: Lazy1 (Channel a a' b b' f) where
    defer1 l = ChanZ (defer l)

  instance functorChannel :: (Functor f) => Functor (Channel a a' b b' f) where
    (<$>) f (Emit  e c q) = Emit e (f <$> c) (f <$> q)
    (<$>) f (Await   g q) = Await ((<$>) f <$> g) (f <$> q)
    (<$>) f (ChanX   x q) = ChanX ((<$>) f <$> x) (f <$> q)
    (<$>) f (ChanZ     z) = ChanZ ((<$>) f <$> z)
    (<$>) f (Stop      r) = Stop (f r)

  instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel a a b b f r) where
    (<>) (Emit e c q) w = Emit e (c <> w) q
    (<>) (Await  f q) w = Await (flip (<>) w <$> f) q
    (<>) (ChanX  x q) w = ChanX (flip (<>) w <$> x) q
    (<>) (ChanZ    z) w = ChanZ (flip (<>) w <$> z)
    (<>) (Stop     r) w = (<>) r <$> w

  instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel a a b b f r) where
    mempty = Stop mempty 

  instance applyChannel :: (Applicative f) => Apply (Channel a a' b b' f) where
    (<*>) (Emit e c q) w = Emit e (c <*> w) (q <*> terminate w)
    (<*>) (Await  g q) w = Await (flip (<*>) w <$> g) (q <*> terminate w)
    (<*>) (ChanX  x q) w = ChanX (flip (<*>) w <$> x) (q <*> terminate w)
    (<*>) (ChanZ    z) w = ChanZ (flip (<*>) w <$> z)
    (<*>) v @ (Stop f) (Emit e c q) = Emit e (v <*> c) (pure f <*> q)
    (<*>) v @ (Stop f) (Await  g q) = Await ((<*>) v <$> g) (pure f <*> q)
    (<*>) v @ (Stop f) (ChanX  x q) = ChanX ((<*>) v <$> x) (pure f <*> q)
    (<*>) v @ (Stop f) (ChanZ    z) = ChanZ ((<*>) v <$> z)
    (<*>) v @ (Stop f) (Stop     x) = Stop (f x)

  instance applicativeChannel :: (Applicative f) => Applicative (Channel a a' b b' f) where
    pure r = Stop r

  instance bindChannel :: (Monad f) => Bind (Channel a a' b b' f) where
    (>>=) (Emit e c q) f = Emit e (c >>= f) (q >>= (terminate <$> f))
    (>>=) (Await  g q) f = Await (flip (>>=) f <$> g) (q >>= (terminate <$> f))
    (>>=) (ChanX  x q) f = ChanX (flip (>>=) f <$> x) (q >>= (terminate <$> f))
    (>>=) (ChanZ    z) f = ChanZ (flip (>>=) f <$> z)
    (>>=) (Stop     r) f = f r

  instance monadChannel :: (Monad f) => Monad (Channel a a' b b' f)

  instance monadTransChannel :: MonadTrans (Channel a a' b b') where 
    lift = yield'