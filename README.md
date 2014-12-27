# Module Documentation

## Module Channels.Core

### Types

    data Channel a a' b b' f r where
      Emit :: Either a' b' -> Channel a a' b b' f r -> Effectable f r -> Channel a a' b b' f r
      Await :: (Either a b -> Channel a a' b b' f r) -> Effectable f r -> Channel a a' b b' f r
      ChanX :: f (Channel a a' b b' f r) -> Effectable f r -> Channel a a' b b' f r
      ChanZ :: Lazy (Channel a a' b b' f r) -> Channel a a' b b' f r
      Stop :: r -> Channel a a' b b' f r

    newtype Downstream b f r a a' where
      Downstream :: Channel a a' b b f r -> Downstream b f r a a'

    data Effectable f a where
      EffPure :: a -> Effectable f a
      EffX :: f a -> Effectable f a
      EffZ :: Lazy (Effectable f a) -> Effectable f a

    type UniChannel a b f r = Channel a a b b f r

    newtype Upstream a f r b b' where
      Upstream :: Channel a a b b' f r -> Upstream a f r b b'


### Type Class Instances

    instance applicativeChannel :: (Applicative f) => Applicative (Channel a a' b b' f)

    instance applicativeEffectable :: (Applicative f) => Applicative (Effectable f)

    instance applyChannel :: (Applicative f) => Apply (Channel a a' b b' f)

    instance applyEffectable :: (Applicative f) => Apply (Effectable f)

    instance bindChannel :: (Monad f) => Bind (Channel a a' b b' f)

    instance bindEffectable :: (Monad f) => Bind (Effectable f)

    instance foldableEffectable :: (Applicative f, Foldable f) => Foldable (Effectable f)

    instance functorChannel :: (Functor f) => Functor (Channel a a' b b' f)

    instance functorEffectable :: (Functor f) => Functor (Effectable f)

    instance lazy1Channel :: Lazy1 (Channel a a' b b' f)

    instance lazy1Effectable :: Lazy1 (Effectable f)

    instance monadChannel :: (Monad f) => Monad (Channel a a' b b' f)

    instance monadEffectable :: (Monad f) => Monad (Effectable f)

    instance monadTransChannel :: MonadTrans (Channel a a' b b')

    instance monadTransEffectable :: MonadTrans Effectable

    instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel a a b b f r)

    instance monoidEffectable :: (Applicative f, Monoid a) => Monoid (Effectable f a)

    instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel a a b b f r)

    instance semigroupEffectable :: (Applicative f, Semigroup a) => Semigroup (Effectable f a)

    instance showEffectable :: (Show (f a), Show a) => Show (Effectable f a)

    instance traversableEffectable :: (Applicative f, Traversable f) => Traversable (Effectable f)


### Values

    await :: forall a a' b b' f r. (Either a b -> Channel a a' b b' f r) -> Effectable f r -> Channel a a' b b' f r

    awaitDown :: forall a a' b f r. (Applicative f) => (a -> Channel a a' b b f r) -> Effectable f r -> Channel a a' b b f r

    awaitUp :: forall a b b' f r. (Applicative f) => (b -> Channel a a b b' f r) -> Effectable f r -> Channel a a b b' f r

    emit :: forall a a' b b' f r. (Applicative f) => Either a' b' -> Effectable f r -> Channel a a' b b' f r

    emitDown :: forall a a' b b' f r. (Applicative f) => b' -> Effectable f r -> Channel a a' b b' f r

    emitUp :: forall a a' b b' f r. (Applicative f) => a' -> Effectable f r -> Channel a a' b b' f r

    prependEffect :: forall a a' b b' f r x. (Functor f, Monoid r) => Channel a a' b b' f r -> f x -> Channel a a' b b' f r

    runEffectable :: forall f a. (Applicative f) => Effectable f a -> f a

    terminate :: forall a a' b b' f r. (Applicative f) => Channel a a' b b' f r -> Effectable f r

    unDownstream :: forall b f r a a'. Downstream b f r a a' -> Channel a a' b b f r

    unUpstream :: forall a f r b b'. Upstream a f r b b' -> Channel a a b b' f r

    yield :: forall a a' b b' f r. r -> Channel a a' b b' f r

    yield' :: forall a a' b b' f r. (Functor f) => f r -> Channel a a' b b' f r



