# Module Documentation

## Module Channels.Bichannel

### Types

    type Bichannel a a' b b' f r = Channel (Either a b) (Either a' b') f r

    type Bisink f a b' r = Bichannel a Z b' Z f r

    type Bisource f a' b r = Bichannel Z a' b Z f r

    type Biworkflow f r = Bichannel Z Z Z Z f r


### Values

    awaitDown :: forall a a' b f. (Monad f) => Bichannel a a' b b f a

    awaitUp :: forall a b b' f. (Monad f) => Bichannel a a b b' f b

    runBiworkflow :: forall f r. (Monad f) => Biworkflow f r -> f r

    stack :: forall a a' a'' b b' b'' f r r'. (Monad f) => Bichannel a a' b' b'' f r -> Bichannel a' a'' b b' f r' -> Bichannel a a'' b b'' f (Tuple (Maybe r) (Maybe r'))

    toDownstream :: forall b f r a a'. (Functor f) => Channel a a' f r -> Bichannel a a' b b f r

    toUpstream :: forall a f r b b'. (Monad f) => Channel b b' f r -> Bichannel a a b b' f r

    toWorkflow :: forall f r. (Monad f) => Biworkflow f r -> Workflow f r

    yieldDown :: forall a a' b b' f. (Applicative f) => a' -> Bichannel a a' b b' f Unit

    yieldUp :: forall a a' b b' f. (Applicative f) => b' -> Bichannel a a' b b' f Unit


## Module Channels.Combinators

### Values

    moore :: forall i o f. (Monad f) => (i -> o) -> Channel i o f Unit

    moore' :: forall i o f. (Monad f) => (i -> f o) -> Channel i o f Unit

    yieldAll :: forall i o f c. (Monad f, Foldable c) => c o -> Channel i o f Unit


## Module Channels.Core

### Types

    data Channel i o f r where
      Yield :: o -> Channel i o f r -> Terminator f r -> Channel i o f r
      Await :: (i -> Channel i o f r) -> Terminator f r -> Channel i o f r
      ChanX :: f (Channel i o f r) -> Channel i o f r
      ChanZ :: Lazy (Channel i o f r) -> Channel i o f r
      Stop :: r -> Channel i o f r

    type Sink i f r = Channel i Z f r

    type Source o f r = Channel Z o f r

    data Terminator f a

    type Workflow f r = Channel Z Z f r

    data Z :: *


### Type Class Instances

    instance altTerminator :: (Functor f) => Alt (Terminator f)

    instance alternativeTerminator :: (Applicative f) => Alternative (Terminator f)

    instance applicativeChannel :: (Monad f) => Applicative (Channel i o f)

    instance applicativeTerminator :: (Applicative f) => Applicative (Terminator f)

    instance applyChannel :: (Monad f) => Apply (Channel i o f)

    instance applyTerminator :: (Applicative f) => Apply (Terminator f)

    instance bindChannel :: (Monad f) => Bind (Channel i o f)

    instance bindTerminator :: (Monad f) => Bind (Terminator f)

    instance functorChannel :: (Functor f) => Functor (Channel i o f)

    instance functorTerminator :: (Functor f) => Functor (Terminator f)

    instance lazy1Channel :: Lazy1 (Channel i o f)

    instance lazy1Terminator :: Lazy1 (Terminator f)

    instance monadChannel :: (Monad f) => Monad (Channel i o f)

    instance monadPlusTerminator :: (Monad f) => Monad (Terminator f)

    instance monadTerminator :: (Monad f) => Monad (Terminator f)

    instance monadTransChannel :: MonadTrans (Channel i o)

    instance monadTransTerminator :: MonadTrans Terminator

    instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r)

    instance monoidTerminator :: (Monad f, Semigroup a) => Monoid (Terminator f a)

    instance plusTerminator :: (Functor f) => Plus (Terminator f)

    instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r)

    instance semigroupTerminator :: (Monad f, Semigroup a) => Semigroup (Terminator f a)

    instance showTerminator :: (Functor f, Show (f String), Show a) => Show (Terminator f a)


### Values

    await :: forall i o f. (Monad f) => Channel i o f i

    compose :: forall a b c f r. (Monad f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r

    finalizer :: forall i o f r x. (Monad f) => f x -> Channel i o f r -> Channel i o f r

    foldChannel :: forall i o f r z. (Monad f) => (o -> Channel i o f r -> Terminator f r -> z) -> ((i -> Channel i o f r) -> Terminator f r -> z) -> (r -> z) -> Channel i o f r -> f z

    loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r

    loopForever :: forall i o f r r'. (Monad f) => Channel i o f r -> Channel i o f r'

    nonTerminator :: forall f a. Terminator f a

    runTerminator :: forall f a. (Monad f) => Terminator f a -> f (Maybe a)

    runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r

    terminate :: forall i o f r. (Monad f) => Channel i o f r -> Terminator f r

    terminateRun :: forall i o f r. (Monad f) => Channel i o f r -> f (Maybe r)

    terminator :: forall i o f r. (Applicative f) => Terminator f r -> Channel i o f r -> Channel i o f r

    wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r

    yield :: forall i o f. (Applicative f) => o -> Channel i o f Unit

    yield' :: forall i o f. (Monad f) => f o -> Channel i o f Unit


## Module Channels.State

### Values

    stateMachine :: forall s i o f. (Monad f) => (s -> i -> Tuple s [o]) -> s -> Channel i o f s

    stateMachine' :: forall s i o f. (Monad f) => (s -> i -> f (Tuple s [o])) -> s -> Channel i o f s


## Module Channels.Stream

### Types

    newtype Stream f r i o where
      Stream :: Channel i o f r -> Stream f r i o


### Type Class Instances

    instance categoryStream :: (Monad f, Semigroup r) => Category (Stream f r)

    instance profunctorStream :: (Monad f) => Profunctor (Stream f r)

    instance semigroupoidStream :: (Monad f, Semigroup r) => Semigroupoid (Stream f r)


### Values

    unStream :: forall f r i o. Stream f r i o -> Channel i o f r



