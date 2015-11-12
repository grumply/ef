# Ef
The Ef programming language

Ef is a multi-paradigm general-purpose programming language. It's built entirely in Haskell and is, at its core, a pure functional language and inherits Haskell's equational reasoning. Using structural subtyping, Ef pairs immutable objects with an imperative-like monad of K-expressions. Ef recovers an object's world line through immutability and enables method invocation or object messaging via an approach called Patterns (K-expressions, continuation expressions) of Symbols. Patterns allow implementation of both language constructs and complex interaction with objects; send a Pattern of methods to an object by way of a, possibly complex and dynamic, composition of messages guaranteed statically to be invokable by that object. Ef has a unique approach to object-oriented programming. Composed methods, or messages, look, themselves, like programs; thus, one might call this style of programming language-oriented or middle-out<sub>1</sub>. Objects define the language they implement and inherit the language of their parents. The duality, or symmetry, between K-expressions and their invocation witness is explicit, rather than implicit as method invocation is in most languages. Performance is quite reasonable and the language is fully compatible with all Haskell libraries, it even allows stacking monad transformers between structural layers of objects! Ef attempts to take the operational/Data Types à la Carte approach to its natural conclusion.

Current work is aimed at a reasonable base of integrated utility libraries, like IO. The language is very much still in a state of flux, so if you see an improvement to be made, let's make it! 

There's plenty left to do. If you're interested in joining the search for the best general-purpose language, jump in!

Note: the name Ef comes from "Effect" or "endless forms" from Darwin's 'On the Origin of Species.'

[1]: (PDF) [M. P. Ward's Language Oriented Programming](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.6369)
