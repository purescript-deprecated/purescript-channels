# Module Documentation

## Module Channels.Bichannel

#### `Bichannel`

``` purescript
type Bichannel a a' b b' f r = Channel (Either a b) (Either a' b') f r
```

A bidirectional channel, which has both upstream and downstream channels
of communication.

#### `Bisource`

``` purescript
type Bisource f a' b r = Bichannel Z a' b Z f r
```

A bisource, defined as a bichannel that never emits upstream values or
awaits downstream values.

#### `Bisink`

``` purescript
type Bisink f a b' r = Bichannel a Z b' Z f r
```

A bisink, defined as a bichannel that never emits downstream values or
awaits upstream values.

#### `Biworkflow`

``` purescript
type Biworkflow f r = Bichannel Z Z Z Z f r
```

A biworkflow, which never awaits or emits upstream or downstream values.

#### `toUpstream`

``` purescript
toUpstream :: forall a f r b b'. (Monad f) => Channel b b' f r -> Bichannel a a b b' f r
```

Converts a stream to an upstream bichannel.

#### `toDownstream`

``` purescript
toDownstream :: forall b f r a a'. (Monad f) => Channel a a' f r -> Bichannel a a' b b f r
```

Converts a stream to a downstream bichannel.

#### `awaitDown`

``` purescript
awaitDown :: forall a a' b f. (Monad f) => Bichannel a a' b b f a
```

Using the specified terminator, awaits a downstream value and passes
through all upstream values.

#### `awaitUp`

``` purescript
awaitUp :: forall a b b' f. (Monad f) => Bichannel a a b b' f b
```

Using the specified terminator, awaits an upstream value and passes
through all downstream values.

#### `yieldDown`

``` purescript
yieldDown :: forall a a' b b' f. (Applicative f) => a' -> Bichannel a a' b b' f Unit
```

Using the specified terminator, emits a downstream value.

#### `yieldUp`

``` purescript
yieldUp :: forall a a' b b' f. (Applicative f) => b' -> Bichannel a a' b b' f Unit
```

Using the specified terminator, emits an upstream value.

#### `stack`

``` purescript
stack :: forall a a' a'' b b' b'' f r r'. (Monad f) => Bichannel a a' b' b'' f r -> Bichannel a' a'' b b' f r' -> Bichannel a a'' b b'' f (Tuple (Maybe r) (Maybe r'))
```

Stacks one bichannel on top of another. Note that if one bichannel
terminates before the other, the second will be forcibly terminated.

Laziness is introduced when channels communicate to each other as a way
of avoiding stack overflows. Of course, for long-running computations,
the base monad must also be trampolined to avoid overflowing the stack.

#### `toWorkflow`

``` purescript
toWorkflow :: forall f r. (Monad f) => Biworkflow f r -> Workflow f r
```

Converts a biworkflow to a workflow.

#### `runBiworkflow`

``` purescript
runBiworkflow :: forall f r. (Monad f) => Biworkflow f r -> f r
```

Runs a biworkflow.


## Module Channels.Combinators

#### `yieldAll`

``` purescript
yieldAll :: forall i o f c. (Monad f, Foldable c) => c o -> Channel i o f Unit
```

Yields a bunch of values.

#### `moore`

``` purescript
moore :: forall i o f. (Monad f) => (i -> o) -> Channel i o f Unit
```

Lifts a pure function to a channel.

#### `moore'`

``` purescript
moore' :: forall i o f. (Monad f) => (i -> f o) -> Channel i o f Unit
```

Lifts an effectful function to a channel.


## Module Channels.Core

#### `Z`

``` purescript
data Z :: *
```


#### `Terminator`

``` purescript
data Terminator f a
```

A channel terminator, which may terminate with a value, or refuse to
terminate. Termination can be accompanied by effects, including
laziness or `f` effects.

#### `Channel`

``` purescript
data Channel i o f r
```

An event-driven channel of communication with a well-defined lifecycle.

Channels may yield output values, await input values, execute effects,
defer computation of a channel, and voluntarily terminate with a final
result value `r`.

All channels may be forcefully terminated to produce an `f (Maybe r)`.

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
feed :: forall i o f r. (Monad f) => i -> Channel i o f r -> Channel i o f (Tuple [i] r)
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
feedAll :: forall i o f r. (Monad f) => [i] -> Channel i o f r -> Channel i o f (Tuple [i] r)
```

Feeds values to the channel. If the channel terminates before it
awaits all the values, the unused values will be monadically returned.

#### `feedAll'`

``` purescript
feedAll' :: forall i o f r. (Monad f) => [i] -> Channel i o f r -> Channel i o f r
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

#### `showTerminator`

``` purescript
instance showTerminator :: (Functor f, Show (f String), Show a) => Show (Terminator f a)
```

#### `lazyTerminator`

``` purescript
instance lazyTerminator :: CL.Lazy (Terminator f a)
```


#### `functorTerminator`

``` purescript
instance functorTerminator :: (Functor f) => Functor (Terminator f)
```


#### `applyTerminator`

``` purescript
instance applyTerminator :: (Applicative f) => Apply (Terminator f)
```

#### `applicativeTerminator`

``` purescript
instance applicativeTerminator :: (Applicative f) => Applicative (Terminator f)
```


#### `bindTerminator`

``` purescript
instance bindTerminator :: (Monad f) => Bind (Terminator f)
```


#### `monadTerminator`

``` purescript
instance monadTerminator :: (Monad f) => Monad (Terminator f)
```


#### `monadTransTerminator`

``` purescript
instance monadTransTerminator :: MonadTrans Terminator
```


#### `semigroupTerminator`

``` purescript
instance semigroupTerminator :: (Monad f, Semigroup a) => Semigroup (Terminator f a)
```


#### `monoidTerminator`

``` purescript
instance monoidTerminator :: (Monad f, Semigroup a) => Monoid (Terminator f a)
```


#### `altTerminator`

``` purescript
instance altTerminator :: (Functor f) => Alt (Terminator f)
```


#### `plusTerminator`

``` purescript
instance plusTerminator :: (Functor f) => Plus (Terminator f)
```


#### `alternativeTerminator`

``` purescript
instance alternativeTerminator :: (Applicative f) => Alternative (Terminator f)
```


#### `monadPlusTerminator`

``` purescript
instance monadPlusTerminator :: (Monad f) => Monad (Terminator f)
```


#### `lazyChannel`

``` purescript
instance lazyChannel :: CL.Lazy (Channel i o f a)
```

#### `functorChannel`

``` purescript
instance functorChannel :: (Functor f) => Functor (Channel i o f)
```


#### `semigroupChannel`

``` purescript
instance semigroupChannel :: (Applicative f, Semigroup r) => Semigroup (Channel io io f r)
```


#### `monoidChannel`

``` purescript
instance monoidChannel :: (Applicative f, Monoid r) => Monoid (Channel io io f r)
```


#### `applyChannel`

``` purescript
instance applyChannel :: (Monad f) => Apply (Channel i o f)
```


#### `applicativeChannel`

``` purescript
instance applicativeChannel :: (Monad f) => Applicative (Channel i o f)
```


#### `bindChannel`

``` purescript
instance bindChannel :: (Monad f) => Bind (Channel i o f)
```


#### `monadChannel`

``` purescript
instance monadChannel :: (Monad f) => Monad (Channel i o f)
```


#### `monadTransChannel`

``` purescript
instance monadTransChannel :: MonadTrans (Channel i o)
```



## Module Channels.Stream

#### `Stream`

``` purescript
newtype Stream f r i o
  = Stream (Channel i o f r)
```

A newtype for Channel so we can define semigroupoid, category,
and profunctor.

#### `unStream`

``` purescript
unStream :: forall f r i o. Stream f r i o -> Channel i o f r
```


#### `semigroupoidStream`

``` purescript
instance semigroupoidStream :: (Monad f, Semigroup r) => Semigroupoid (Stream f r)
```


#### `categoryStream`

``` purescript
instance categoryStream :: (Monad f, Semigroup r) => Category (Stream f r)
```


#### `profunctorStream`

``` purescript
instance profunctorStream :: (Monad f) => Profunctor (Stream f r)
```




