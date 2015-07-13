## Module Channels.Stream

#### `Stream`

``` purescript
newtype Stream f r i o
  = Stream (Channel i o f r)
```

A newtype for Channel so we can define semigroupoid, category, 
and profunctor.

##### Instances
``` purescript
instance semigroupoidStream :: (Monad f, Semigroup r) => Semigroupoid (Stream f r)
instance categoryStream :: (Monad f, Semigroup r) => Category (Stream f r)
instance profunctorStream :: (Monad f) => Profunctor (Stream f r)
```

#### `unStream`

``` purescript
unStream :: forall f r i o. Stream f r i o -> Channel i o f r
```


