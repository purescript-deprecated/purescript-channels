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


