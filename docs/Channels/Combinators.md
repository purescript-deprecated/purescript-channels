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


