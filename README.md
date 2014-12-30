# Module Documentation

## Module Channels.Bichannel

### Types

    type Bichannel a a' b b' f r = Channel (Either a b) (Either a' b') f r

    type Bisink f a b r = Bichannel a Unit Unit b f r

    type Bisource f a b r = Bichannel Unit a b Unit f r

    type Biworkflow f r = Bichannel Unit Unit Unit Unit f r


### Values

    awaitDown :: forall a a' b f r. (Applicative f) => Effectable f r -> (a -> Bichannel a a' b b f r) -> Bichannel a a' b b f r

    awaitUp :: forall a b b' f r. (Applicative f) => Effectable f r -> (b -> Bichannel a a b b' f r) -> Bichannel a a b b' f r

    runBiworkflow :: forall f r. (Monad f) => Biworkflow f r -> f r

    stack :: forall a a' a'' b b' b'' f r r'. (Applicative f) => Bichannel a a' b' b'' f r -> Bichannel a' a'' b b' f r' -> Bichannel a a'' b b'' f (Tuple r r')

    toDownstream :: forall b f r a a'. (Functor f) => Channel a a' f r -> Bichannel a a' b b f r

    toUpstream :: forall a f r b b'. (Functor f) => Channel b b' f r -> Bichannel a a b b' f r

    toWorkflow :: forall f r. (Applicative f) => Biworkflow f r -> Workflow f r

    yieldDown :: forall a a' b b' f r. (Applicative f) => Effectable f r -> a' -> Bichannel a a' b b' f r

    yieldUp :: forall a a' b b' f r. (Applicative f) => Effectable f r -> b' -> Bichannel a a' b b' f r


## Module Channels.Combinators

### Values

    yieldAll :: forall i o f c. (Applicative f, Foldable c) => c o -> Channel i o f [o]


## Module Channels.Core

### Types

    data Channel i o f r where
      Yield :: o -> Channel i o f r -> Effectable f r -> Channel i o f r
      Await :: (i -> Channel i o f r) -> Effectable f r -> Channel i o f r
      ChanX :: f (Channel i o f r) -> Effectable f r -> Channel i o f r
      ChanZ :: Lazy (Channel i o f r) -> Channel i o f r
      Stop :: r -> Channel i o f r

    data Effectable f a

    type Sink i f r = Channel i Unit f r

    type Source o f r = Channel Unit o f r

    type Workflow f r = Channel Unit Unit f r


### Type Class Instances

    instance applicativeChannel :: (Applicative f) => Applicative (Channel i o f)

    instance applicativeEffectable :: (Applicative f) => Applicative (Effectable f)

    instance applyChannel :: (Applicative f) => Apply (Channel i o f)

    instance applyEffectable :: (Applicative f) => Apply (Effectable f)

    instance bindChannel :: (Monad f) => Bind (Channel i o f)

    instance bindEffectable :: (Monad f) => Bind (Effectable f)

    instance foldableEffectable :: (Applicative f, Foldable f) => Foldable (Effectable f)

    instance functorChannel :: (Functor f) => Functor (Channel i o f)

    instance functorEffectable :: (Functor f) => Functor (Effectable f)

    instance lazy1Channel :: Lazy1 (Channel i o f)

    instance lazy1Effectable :: Lazy1 (Effectable f)

    instance monadChannel :: (Monad f) => Monad (Channel i o f)

    instance monadEffectable :: (Monad f) => Monad (Effectable f)

    instance monadTransChannel :: MonadTrans (Channel i o)

    instance monadTransEffectable :: MonadTrans Effectable

    instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r)

    instance monoidEffectable :: (Applicative f, Monoid a) => Monoid (Effectable f a)

    instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r)

    instance semigroupEffectable :: (Applicative f, Semigroup a) => Semigroup (Effectable f a)

    instance showEffectable :: (Show (f a), Show a) => Show (Effectable f a)

    instance traversableEffectable :: (Applicative f, Traversable f) => Traversable (Effectable f)


### Values

    await :: forall i o f r. Effectable f r -> (i -> Channel i o f r) -> Channel i o f r

    compose :: forall a b c f r. (Applicative f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r

    finalizer :: forall i o f r x. (Applicative f) => f x -> Channel i o f r -> Channel i o f r

    loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r

    moore :: forall f r i o. (Applicative f, Monoid r) => (i -> o) -> Channel i o f r

    moore' :: forall f r i o. (Applicative f, Monoid r) => (i -> f o) -> Channel i o f r

    runEffectable :: forall f a. (Applicative f) => Effectable f a -> f a

    runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r

    stop :: forall i o f r. r -> Channel i o f r

    stop' :: forall i o f r. (Functor f) => f r -> Channel i o f r

    terminate :: forall i o f r. (Applicative f) => Channel i o f r -> Effectable f r

    terminator :: forall i o f r. (Applicative f) => Effectable f r -> Channel i o f r -> Channel i o f r

    wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r

    yield :: forall i o f r. (Applicative f) => Effectable f r -> o -> Channel i o f r

    yield' :: forall i o f r. (Applicative f) => Effectable f r -> f o -> Channel i o f r


## Module Channels.State

### Values

    stateMachine :: forall s i o f. (Applicative f) => (s -> i -> Tuple s [o]) -> s -> Channel i o f s

    stateMachine' :: forall s i o f. (Monad f) => (s -> i -> f (Tuple s [o])) -> s -> Channel i o f s


## Module Channels.Stream

### Types

    newtype Stream f r i o where
      Stream :: Channel i o f r -> Stream f r i o


### Type Class Instances

    instance categoryStream :: (Applicative f, Monoid r) => Category (Stream f r)

    instance profunctorStream :: (Applicative f) => Profunctor (Stream f r)

    instance semigroupoidStream :: (Applicative f, Semigroup r) => Semigroupoid (Stream f r)


### Values

    unStream :: forall f r i o. Stream f r i o -> Channel i o f r



