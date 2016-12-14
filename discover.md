I had a fun refactoring example in Haskell today I wanted to share.
So, I've got a structure with a nested `Maybe` inside, which looked like this:

```hs
Maybe (Vector.Vector (Maybe (Direction, [Departure])))
```

I wanted to get that second-level `Maybe` folded into the first as it didn't provide any semantic meaning.
So I start by writing the type definition:

```hs
seqVec :: Maybe (Vector.Vector (Maybe (Direction, [Departure])))
       -> Maybe (Vector.Vector (Direction, [Departure]))
```

When I don't quite know how to approach a problem first, I try and take the
easiest, hackiest way first and go with a `do` block:

```hs
seqVec mvec = do
  vec <- mvec
  i <- sequence vec
  return i
```

This works and is pretty clear, but also super redundant. So let's fold the last
two lines into one as a first step:


```hs
seqVec' mvec = do
  vec <- mvec
  sequence vec
```

This makes it pretty clear, that the `do` notation here might be overkill.


```hs
seqVec'' mvec = mvec >>= \vec -> sequence vec
```

Now this is just screaming for eta reduction.

```hs
seqVec''' mvec = mvec >>= sequence
```

One more step and there's barely anything left of this function.

```hs
seqVec'''' = (>>= sequence)
```

And clearly this is useful in a broader scope than just my nested
Vector example. What does GHCI infer for this?

```hs
λ> :t (>>= sequence)
(>>= sequence)
  :: (Traversable t, Monad m) => m (t (m a)) -> m (t a)
```

Haskell is one of the few languages where I find refactoring
truly enjoyable and this is a major reason why. 
