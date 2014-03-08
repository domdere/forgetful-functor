---
title: Adjunctions and the State Monad
---

## Background

### Adjunctions, Monads and Comonads

There are many equivalent definitions around for an adjunction between categories, my favourite is the following
(from *Categories for the Working Mathematician* by Saunders Mac Lane):

>   Let `A` and `X` be categories, an adjunction from `X` to `A` is a triple `<F, G, phi> : A -> X`, where `F` and `G`
>   are functors, `F : X -> A` and `G : A -> X` while `phi` is a function which assigns to each `x in X` and `a in A`
>   a bijection of sets `phi(x, a) : Hom(Fx, a) <-> Hom(x, Ga)` which is natural in `x` and `a`.

I'll try to describe what naturality means later when I use it. However if I fail at that, a proper definition can be found
[here] [natural-isos].

`T : X -> X`, with `T = G . F`, is a monad on `X` while `F . G` is a comonad on `A`.  Its stated here handwavily, but the intention
is that the math will be fleshed out as we go through the example.

Its also a fact that every monad on a category arises from some adjunction.  I wont be justifying this statement in this post,
but for those interested, they are referred to the following Wikipedia entries:

1.  [**The Kleisli Category**] [kleisli-cats]: In the category of adjunctions which generate a given monad `T`, the adjunction implied by this construction is an initial object in this category
2.  [**The Eilenburg-Moore Category**] [eilenburg-moore]: This category forms a terminal object in the aforementioned category.

-------

## The State Monad

Once you read about these facts, the prospect of deconstructing all the bread and butter monads into adjunctions seems pretty exciting.
For any given monad `T = G . F`, if `F` is non-trivial (i.e not equal to the Identity or Forgetful functor or to `T` itself), it offers insight
into some sort of intermediate step to construct the monad.

Unfortunately, most of the common monads (`Reader`, `Writer`, `Maybe`, `[]`, etc..), do not have non-trivial adjunctions that provide this kind of insight (though
they do have adjunctions that do provide interesting insights about the key properties that define them).

Another problem in terms of finding an adjunction that can be expressed in Haskell without complicated tricks is that all instances of
`Functor`, `Monad` and `Comonad` are endofunctors on (Proper) Hask.  So if you wish to find an adjunction for a Haskell `Monad` such that it generates
a comonad that is an instance of a Haskell `Comonad`, both `A` and `X` have to be (Proper) Hask.  The most interesting and common example of a non-trivial
adjunction that results in a Haskell `Comonad` is an adjunction for the `State` monad that gives rise to the `Store` Comonad.

### The Common Definitions for State and Store

The `State` monad definition in `Control.Monad.State` looks something like this:

``` Haskell
data State' s a = State' { runState' :: s -> (s, a) }
```

with a `Functor` instance that looks something like this:

``` Haskell
instance Functor (State' s) where
    f `fmap` (State' g) = State' $ sndMap f . g
        where
            sndMap :: (a -> b) -> (c, a) -> (c, b)
            sndMap func (x, y) = (x, func y)
```

and a `Monad` instance that looks a bit like this (though its probably written better):

``` Haskell
instance Monad (State' s) where
    return x = State' $ \s -> (s, x)

    mx >>= f = State' $ \s -> let
        (s', x) = runState' mx s
        in runState' (f x) s'
```
[natural-isos]: http://en.wikipedia.org/wiki/Natural_isomorphism#natural_isomorphism "Natural Isomorphisms on Wikipedia"
[kleisli-cats]: http://en.wikipedia.org/wiki/Kleisli_categories "Kleisli Categories on Wikipedia"
[eilenburg-moore]: http://en.wikipedia.org/wiki/Eilenberg%E2%80%93Moore_category#Algebras_for_a_monad "Eilenburg-Moore Categories on Wikipedia"
