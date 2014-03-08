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

Once you read about these facts, the first impulse is to go and deconstruct all the fundamental monads we commonly use in FP.

[natural-isos]: http://en.wikipedia.org/wiki/Natural_isomorphism#natural_isomorphism "Natural Isomorphisms on Wikipedia"
[monads-adjs]: 
[kleisli-cats]: http://en.wikipedia.org/wiki/Kleisli_categories "Kleisli Categories on Wikipedia"
[eilenburg-moore]: http://en.wikipedia.org/wiki/Eilenberg%E2%80%93Moore_category#Algebras_for_a_monad "Eilenburg-Moore Categories on Wikipedia"
