module Channels.Core
  ( Channel()
  , Terminator()
  , Sink(..)
  , Source(..)
  , Workflow(..)
  , Z()
  , (!:), replaceTerminator
  , await
  , compose
  , feed
  , feed'
  , feedAll
  , feedAll'
  , finalizer
  , foldChannel
  , loop
  , loopForever
  , nonTerminator
  , runTerminator
  , runWorkflow
  , terminate
  , terminateRun
  , wrapEffect
  , yield
  , yield'
  ) where

  import Prelude (class Monad, class Bind, class Applicative, class Apply, class Semigroup, class Functor, class Show, Unit, pure, (<$>), (>>=), apply, (<*>), append, (<>), map, show, (++), unit, (>>>), (<<<), flip)

  import Data.Maybe(Maybe(..), maybe)
  import Data.Monoid(class Monoid, mempty)
  import Data.Tuple(Tuple(..), snd)
  import Data.Lazy(Lazy, force)
  import Data.List(List(..), singleton)

  import Control.Alt (class Alt, alt, (<|>))
  import Control.Alternative(class Alternative)
  import Control.Apply ((*>))
  import Control.Lazy(class Lazy, defer)
  import Control.Monad.Trans(class MonadTrans, lift)
  import Control.Plus (class Plus)

  -- | An uninhabited type
  newtype Z = Z (Unit -> Z)

  unsafeZ :: Z
  unsafeZ = Z \_ -> unsafeZ

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
    | ChanX (f (Channel i o f r))
    | ChanZ (Lazy (Channel i o f r))
    | Stop r

  -- | A source of values, which awaits nothing.
  type Source o f r = Channel Z o f r

  -- | A sink of values, which emits nothing.
  type Sink i f r = Channel i Z f r

  -- | A workflow consists of a source composed with a sink.
  type Workflow f r = Channel Z Z f r

  infixl 4 replaceTerminator as !:

  nonTerminator :: forall f a. Terminator f a
  nonTerminator = TerE

  -- | Pipes the output of one channel to the input of another.
  compose :: forall a b c f r. (Monad f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r
  compose c1 (Await f2 q2)    = Await (compose c1 <$> f2) (q2 <> terminate c1)
  compose (Yield o c1 q1) c2  = Yield o (c1 `compose` c2) (terminate c2 <> q1)
  compose c1 (ChanX fc2)      = ChanX (compose c1 <$> fc2)
  compose c1 (ChanZ zc2)      = ChanZ (compose c1 <$> zc2)
  compose (ChanX fc1) c2      = ChanX (flip compose c2 <$> fc1)
  compose (ChanZ zc1) c2      = ChanZ (flip compose c2 <$> zc1)
  compose (Stop r1) c2        = lift (maybe r1 (flip (<>) r1) <$> terminateRun c2)
  compose c1 (Stop r2)        = lift (maybe r2 (     (<>) r2) <$> terminateRun c1)
  compose (Await f1 _) (Yield o c2 _) = defer \_ -> f1 o `compose` c2

  -- | Runs an terminator to produce an `f (Maybe a)`.
  runTerminator :: forall f a. (Monad f) => Terminator f a -> f (Maybe a)
  runTerminator (TerP  a) = pure (Just a)
  runTerminator TerE      = pure (Nothing)
  runTerminator (TerX fa) = fa >>= runTerminator
  runTerminator (TerZ ef) = runTerminator (force ef)

  -- | Runs a workflow to completion. TODO: stack overflow.
  runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r
  runWorkflow w = loop' w
    where loop' (Yield _ c _) = loop' c
          loop' (Await   f _) = loop' (f unsafeZ)
          loop' (ChanX     x) = x >>= loop'
          loop' (ChanZ     z) = loop' (force z)
          loop' (Stop      r) = pure r

  -- | Returns a looping channel that restarts when it reaches the end.
  loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r
  loop c0 = loop' c0
    where loop' (Yield o c q) = Yield o (loop' c) q
          loop' (Await   f q) = Await (loop' <$> f) q
          loop' (ChanX     x) = ChanX (loop' <$> x)
          loop' (ChanZ     z) = ChanZ (loop' <$> z)
          loop' (Stop      _) = loop' c0


  -- | Returns a looping channel that will refuse to terminate.
  loopForever :: forall i o f r r'. (Monad f) => Channel i o f r -> Channel i o f r'
  loopForever c0 = loop' c0
    where loop' (Yield o c q) = Yield o (loop' c) (q *> nonTerminator)
          loop' (Await   f q) = Await (loop' <$> f) (q *> nonTerminator)
          loop' (ChanX     x) = ChanX (loop' <$> x)
          loop' (ChanZ     z) = ChanZ (loop' <$> z)
          loop' (Stop      r) = loop' c0

  -- | Awaits a value and monadically returns it.
  await :: forall i o f. (Monad f) => Channel i o f i
  await = Await pure nonTerminator

  -- | Yields a value.
  yield :: forall i o f. (Applicative f) => o -> Channel i o f Unit
  yield o = Yield o stopUnit terminatorUnit

  -- | Using the specified terminator, yields an effectful value.
  yield' :: forall i o f. (Monad f) => f o -> Channel i o f Unit
  yield' = wrapEffect <<< ((<$>) yield)

  -- | Feeds a value to the channel. If the channel terminates before it
  -- | awaits the value, the value will be monadically returned.
  feed :: forall i o f r. (Monad f) => i -> Channel i o f r -> Channel i o f (Tuple (List i) r)
  feed i = feedAll (singleton i)

  -- | Feeds a value to the channel. If the channel terminates before it
  -- | awaits the value, the value will be discarded.
  feed' :: forall i o f r. (Monad f) => i -> Channel i o f r -> Channel i o f r
  feed' i = feedAll' (singleton i)

  -- | Feeds values to the channel. If the channel terminates before it
  -- | awaits all the values, the unused values will be monadically returned.
  feedAll :: forall i o f r. (Monad f) => List i -> Channel i o f r -> Channel i o f (Tuple (List i) r)
  feedAll = loop'
    where
      loop' Nil         c             = Tuple Nil <$> c
      loop' is          (Yield o c q) = Yield o (loop' is c) (Tuple is <$> q)
      loop' (Cons i is) (Await   f q) = feedAll is (f i)
      loop' is          (ChanX     x) = ChanX (loop' is <$> x)
      loop' is          (ChanZ     z) = ChanZ (loop' is <$> z)
      loop' is          (Stop      r) = Stop (Tuple is r)

  -- | Feeds values to the channel. If the channel terminates before it
  -- | awaits all the values, the unused values will be discarded.
  feedAll' :: forall i o f r. (Monad f) => List i -> Channel i o f r -> Channel i o f r
  feedAll' is = (<$>) snd <<< feedAll is

  -- | Wraps an effect into the channel.
  wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r
  wrapEffect = ChanX

  -- | Forcibly terminates a channel and returns a terminator.
  terminate :: forall i o f r. (Monad f) => Channel i o f r -> Terminator f r
  terminate (Yield _ _ q) = q
  terminate (Await   _ q) = q
  terminate (ChanX     x) = lift x >>= terminate
  terminate (ChanZ     l) = defer \_ -> (terminate (force l))
  terminate (Stop      r) = pure r

  -- | Terminates a channel and runs the terminator.
  terminateRun :: forall i o f r. (Monad f) => Channel i o f r -> f (Maybe r)
  terminateRun = terminate >>> runTerminator

  -- | Replaces the value that the channel will produce if forcibly terminated.
  -- | Preserves any effects associated with the old terminator.
  replaceTerminator :: forall i o f r. (Applicative f) => Channel i o f r -> Terminator f r -> Channel i o f r
  replaceTerminator c q2 = loop' c
    where
      loop' (Yield o c q1) = Yield o (loop' c) (q1 *> q2)
      loop' (Await   f q1) = Await (loop' <$> f) (q1 *> q2)
      loop' (ChanX      x) = ChanX (loop' <$> x)
      loop' (ChanZ      z) = ChanZ (loop' <$> z)
      loop' (Stop       r) = Stop r

  -- | Attaches the specified finalizer to the channel. The finalizer will be
  -- | called when the channel is forcibly terminated or when it voluntarily
  -- | terminates (but just once).
  finalizer :: forall i o f r x. (Monad f) => f x -> Channel i o f r -> Channel i o f r
  finalizer x = loop'
    where
      x' = TerX (TerP <$> x)

      loop' (Yield o c q) = Yield o (loop' c) (x' *> q)
      loop' (Await   f q) = Await (loop' <$> f) (x' *> q)
      loop' (ChanX     x) = ChanX (loop' <$> x)
      loop' (ChanZ     z) = ChanZ (loop' <$> z)
      loop' (Stop      r) = lift x *> Stop r

  -- | Folds over the top-level structure of the channel to produce a value.
  -- | Note that this currently destroys laziness (!).
  foldChannel :: forall i o f r z. (Monad f) =>
    (o -> Channel i o f r -> Terminator f r -> z) ->
    ((i -> Channel i o f r) -> Terminator f r -> z) ->
    (r -> z) ->
    Channel i o f r ->
    f z
  foldChannel yieldF awaitF stopF = loop'
    where loop' (Yield o c q) = pure (yieldF o c q)
          loop' (Await   h q) = pure (awaitF h q)
          loop' (ChanX     x) = x >>= loop'
          loop' (ChanZ     z) = loop' (force z)
          loop' (Stop      r) = pure (stopF r)

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

  instance lazyTerminator :: Lazy (Terminator f a) where
    defer l = TerZ (Data.Lazy.defer l)

  instance functorTerminator :: (Functor f) => Functor (Terminator f) where
    map f (TerP a) = TerP (f a)
    map f TerE     = TerE
    map f (TerX x) = TerX (map f <$> x)
    map f (TerZ z) = TerZ (map f <$> z)

  -- TODO: Implement apply and bind more efficiently!
  instance applyTerminator :: (Applicative f) => Apply (Terminator f) where
    apply (TerP f) (TerP x) = TerP (f x)
    apply TerE            _ = TerE
    apply _            TerE = TerE
    apply (TerX f)        x = TerX ((<*> x) <$> f)
    apply (TerZ f)        x = TerZ ((<*> x) <$> f)
    apply f        (TerX x) = TerX (apply f <$> x)
    apply f        (TerZ x) = TerZ (apply f <$> x)

  instance applicativeTerminator :: (Applicative f) => Applicative (Terminator f) where
    pure a = TerP a

  instance bindTerminator :: (Monad f) => Bind (Terminator f) where
    bind (TerP a) f = f a
    bind TerE     f = TerE
    bind (TerX x) f = TerX ((>>= f) <$> x)
    bind (TerZ z) f = defer \_ -> force z >>= f

  instance monadTerminator :: (Monad f) => Monad (Terminator f)

  instance monadTransTerminator :: MonadTrans Terminator where
    lift fa = TerX (TerP <$> fa)

  instance semigroupTerminator :: (Monad f, Semigroup a) => Semigroup (Terminator f a) where
    append x y = defer \_ -> TerX (maybe TerE TerP <$> ((<>) <$> runTerminator x <*> runTerminator y))

  instance monoidTerminator :: (Monad f, Semigroup a) => Monoid (Terminator f a) where
    mempty = TerE

  instance altTerminator :: (Functor f) => Alt (Terminator f) where
    alt TerE y = y
    alt x TerE = x
    alt (TerX x) y = TerX ((<|> y) <$> x)
    alt (TerZ x) y = TerZ ((<|> y) <$> x)
    alt x (TerX y) = TerX (alt x <$> y)
    alt x (TerZ y) = TerZ (alt x <$> y)
    alt x y = x

  instance plusTerminator :: (Functor f) => Plus (Terminator f) where
    empty = TerE

  instance alternativeTerminator :: (Applicative f) => Alternative (Terminator f)

  instance monadPlusTerminator :: (Monad f) => Monad (Terminator f)

  -- Channel instances
  instance lazyChannel :: Lazy (Channel i o f r) where
    defer l = ChanZ (Data.Lazy.defer l)

  instance functorChannel :: (Functor f) => Functor (Channel i o f) where
    map f (Yield o c q) = Yield o (f <$> c) (f <$> q)
    map f (Await   g q) = Await (map f <$> g) (f <$> q)
    map f (ChanX     x) = ChanX (map f <$> x)
    map f (ChanZ     z) = ChanZ (map f <$> z)
    map f (Stop      r) = Stop (f r)

  instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r) where
    append (Yield o c q) w = Yield o (c <> w) q
    append (Await   f q) w = Await ((<> w) <$> f) q
    append (ChanX     x) w = ChanX ((<> w) <$> x)
    append (ChanZ     z) w = ChanZ ((<> w) <$> z)
    append (Stop      r) w = append r <$> w

  instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r) where
    mempty = Stop mempty

  instance applyChannel :: (Monad f) => Apply (Channel i o f) where
    apply (Yield o c q) w = Yield o (c <*> w) (q <*> terminate w)
    apply (Await   g q) w = Await ((<*> w) <$> g) (q <*> terminate w)
    apply (ChanX     x) w = ChanX ((<*> w) <$> x)
    apply (ChanZ     z) w = ChanZ ((<*> w) <$> z)
    apply v @ (Stop f) (Yield o c q) = Yield o (v <*> c) (pure f <*> q)
    apply v @ (Stop f) (Await  g q)  = Await (apply v <$> g) (pure f <*> q)
    apply v @ (Stop f) (ChanX    x)  = ChanX (apply v <$> x)
    apply v @ (Stop f) (ChanZ    z)  = ChanZ (apply v <$> z)
    apply v @ (Stop f) (Stop     x)  = Stop (f x)

  instance applicativeChannel :: (Monad f) => Applicative (Channel i o f) where
    pure r = Stop r

  instance bindChannel :: (Monad f) => Bind (Channel i o f) where
    bind (Yield o c q) f = Yield o (c >>= f) (q >>= (terminate <$> f))
    bind (Await   g q) f = Await ((>>= f) <$> g) (q >>= (terminate <$> f))
    bind (ChanX     x) f = ChanX ((>>= f) <$> x)
    bind (ChanZ     z) f = ChanZ ((>>= f) <$> z)
    bind (Stop      r) f = f r

  instance monadChannel :: (Monad f) => Monad (Channel i o f)

  instance monadTransChannel :: MonadTrans (Channel i o) where
    lift fr = wrapEffect (pure <$> fr)
