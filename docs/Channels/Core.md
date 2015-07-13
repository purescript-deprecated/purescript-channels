## Module Channels.Core

#### `Z`

``` purescript
newtype Z
```

An uninhabited type

#### `Terminator`

``` purescript
data Terminator f a
```

A channel terminator, which may terminate with a value, or refuse to 
terminate. Termination can be accompanied by effects, including 
laziness or `f` effects.

##### Instances
``` purescript
instance showTerminator :: (Functor f, Show (f String), Show a) => Show (Terminator f a)
instance lazyTerminator :: Lazy (Terminator f a)
instance functorTerminator :: (Functor f) => Functor (Terminator f)
instance applyTerminator :: (Applicative f) => Apply (Terminator f)
instance applicativeTerminator :: (Applicative f) => Applicative (Terminator f)
instance bindTerminator :: (Monad f) => Bind (Terminator f)
instance monadTerminator :: (Monad f) => Monad (Terminator f)
instance monadTransTerminator :: MonadTrans Terminator
instance semigroupTerminator :: (Monad f, Semigroup a) => Semigroup (Terminator f a)
instance monoidTerminator :: (Monad f, Semigroup a) => Monoid (Terminator f a)
instance altTerminator :: (Functor f) => Alt (Terminator f)
instance plusTerminator :: (Functor f) => Plus (Terminator f)
instance alternativeTerminator :: (Applicative f) => Alternative (Terminator f)
instance monadPlusTerminator :: (Monad f) => Monad (Terminator f)
```

#### `Channel`

``` purescript
data Channel i o f r
```

An event-driven channel of communication with a well-defined lifecycle.

Channels may yield output values, await input values, execute effects, 
defer computation of a channel, and voluntarily terminate with a final
result value `r`.

All channels may be forcefully terminated to produce an `f (Maybe r)`.

##### Instances
``` purescript
instance lazyChannel :: Lazy (Channel i o f r)
instance functorChannel :: (Functor f) => Functor (Channel i o f)
instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r)
instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r)
instance applyChannel :: (Monad f) => Apply (Channel i o f)
instance applicativeChannel :: (Monad f) => Applicative (Channel i o f)
instance bindChannel :: (Monad f) => Bind (Channel i o f)
instance monadChannel :: (Monad f) => Monad (Channel i o f)
instance monadTransChannel :: MonadTrans (Channel i o)
```

#### `Source`

``` purescript
type Source o f r = Channel Z o f r
```

A source of values, which awaits nothing.

#### `Sink`

``` purescript
type Sink i f r = Channel i Z f r
```

A sink of values, which emits nothing.

#### `Workflow`

``` purescript
type Workflow f r = Channel Z Z f r
```

A workflow consists of a source composed with a sink.

#### `nonTerminator`

``` purescript
nonTerminator :: forall f a. Terminator f a
```

#### `compose`

``` purescript
compose :: forall a b c f r. (Monad f, Semigroup r) => Channel b c f r -> Channel a b f r -> Channel a c f r
```

Pipes the output of one channel to the input of another.

#### `runTerminator`

``` purescript
runTerminator :: forall f a. (Monad f) => Terminator f a -> f (Maybe a)
```

Runs an terminator to produce an `f (Maybe a)`.

#### `runWorkflow`

``` purescript
runWorkflow :: forall f r. (Monad f) => Workflow f r -> f r
```

Runs a workflow to completion. TODO: stack overflow.

#### `loop`

``` purescript
loop :: forall i o f r. (Functor f) => Channel i o f r -> Channel i o f r
```

Returns a looping channel that restarts when it reaches the end.

#### `loopForever`

``` purescript
loopForever :: forall i o f r r'. (Monad f) => Channel i o f r -> Channel i o f r'
```

Returns a looping channel that will refuse to terminate.

#### `await`

``` purescript
await :: forall i o f. (Monad f) => Channel i o f i
```

Awaits a value and monadically returns it.

#### `yield`

``` purescript
yield :: forall i o f. (Applicative f) => o -> Channel i o f Unit
```

Yields a value.

#### `yield'`

``` purescript
yield' :: forall i o f. (Monad f) => f o -> Channel i o f Unit
```

Using the specified terminator, yields an effectful value.

#### `feed`

``` purescript
feed :: forall i o f r. (Monad f) => i -> Channel i o f r -> Channel i o f (Tuple (List i) r)
```

Feeds a value to the channel. If the channel terminates before it 
awaits the value, the value will be monadically returned.

#### `feed'`

``` purescript
feed' :: forall i o f r. (Monad f) => i -> Channel i o f r -> Channel i o f r
```

Feeds a value to the channel. If the channel terminates before it 
awaits the value, the value will be discarded.

#### `feedAll`

``` purescript
feedAll :: forall i o f r. (Monad f) => List i -> Channel i o f r -> Channel i o f (Tuple (List i) r)
```

Feeds values to the channel. If the channel terminates before it 
awaits all the values, the unused values will be monadically returned.

#### `feedAll'`

``` purescript
feedAll' :: forall i o f r. (Monad f) => List i -> Channel i o f r -> Channel i o f r
```

Feeds values to the channel. If the channel terminates before it 
awaits all the values, the unused values will be discarded.

#### `wrapEffect`

``` purescript
wrapEffect :: forall i o f r. (Monad f) => f (Channel i o f r) -> Channel i o f r
```

Wraps an effect into the channel.

#### `terminate`

``` purescript
terminate :: forall i o f r. (Monad f) => Channel i o f r -> Terminator f r
```

Forcibly terminates a channel and returns a terminator.

#### `terminateRun`

``` purescript
terminateRun :: forall i o f r. (Monad f) => Channel i o f r -> f (Maybe r)
```

Terminates a channel and runs the terminator.

#### `(!:)`

``` purescript
(!:) :: forall i o f r. (Applicative f) => Channel i o f r -> Terminator f r -> Channel i o f r
```

_left-associative / precedence 4_

Replaces the value that the channel will produce if forcibly terminated.
Preserves any effects associated with the old terminator.

#### `finalizer`

``` purescript
finalizer :: forall i o f r x. (Monad f) => f x -> Channel i o f r -> Channel i o f r
```

Attaches the specified finalizer to the channel. The finalizer will be
called when the channel is forcibly terminated or when it voluntarily 
terminates (but just once).

#### `foldChannel`

``` purescript
foldChannel :: forall i o f r z. (Monad f) => (o -> Channel i o f r -> Terminator f r -> z) -> ((i -> Channel i o f r) -> Terminator f r -> z) -> (r -> z) -> Channel i o f r -> f z
```

Folds over the top-level structure of the channel to produce a value.
Note that this currently destroys laziness (!).


