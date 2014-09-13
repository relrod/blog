---
title: It's not about Haskell
author: Ricky Elrod
date: September 01, 2014
tags: haskell, functional programming, compsci
---

Sometimes on Twitter and IRC, I see people say to my friends within the
functional programming community (or to me directly) something like "It's not
<language> so it must suck, right?" This line of reasoning is a
[fallacy](https://yourlogicalfallacyis.com/strawman), but I wish to debunk it
beyond passing it off as "fallacy, go away!"

If you hang around the right places with me on IRC, you'll see that I sometimes
am very straightforward with my feelings about various bits of technology,
computer science, and programming paradigms and languages. In 2011 or so, I
started getting into functional programming, and haven't looked back. It is
both what I enjoy learning about and what enables me to
[reason](http://www.haskellforall.com/2013/12/equational-reasoning.html) about
what my code is actually doing.

I will say it: I think the state of the software industry needs a lot of work
right now. It is literally daily throughout the course of school and work that
I am either first-hand bitten by a software "bug" or subject to seeing someone
be bit by one, and not being able to do anything for them except feel bad for
them, on behalf of our industry. It is a sorry place to be, but **I would like
to strive to make it better**. Every person who cares, even just a little bit,
helps.

This is why I find it hurtful when people say things like "It's not Haskell, so
it must suck, right?" - It **completely misses the point of the problem**! When
I look at some new language or technology, I can *promise* you that my first
thought is not "How different is this from my perfect little ivory tower of
Haskell code?" My first thought is often "Does this work? Like, *actually*
work?" Often the answer is "no," but occasionally the answer is "no, but it
looks like a good start." Rarely, but sometimes, the answer is a
[resounding](http://goto.ucsd.edu/quark/) ["yes!"](http://sel4.systems/)

What does it mean for a piece of software to "work"? This is an interesting
question to take some time and think about. I would claim that in order for
something to "work," it must be *correct*. That is, given a specification and
an implementation of it, the implementation must exactly do what the
specification says - nothing more, nothing less.

Proving things to be correct is hard. When we use good tooling, we can be aided
in formalizing our proofs by making use of things such as
[parametricity](https://dl.dropboxusercontent.com/u/7810909/talks/20140513/parametricity.pdf)
and the
[Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry-Howard). This
allows us to have assurance that our code does what is supposed to. Sometimes we
have to play
[fast and loose](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html)
but (as that paper says) we are morally correct to do so.

There are other ways of achieving this goal. Automated theorem provers, for
example - although many of these tend to rely on the same principles mentioned
above.

To cut to the chase, I care very little *how* you arrive at "correct software,"
only that you manage to, somehow. Making use of functional programming is by far
the easiest way I have found to even come close to doing this, and that is what
makes me so interested in it. It's not about Haskell or any other particular
language. It's about wanting my software (and software I use) to work. Please
stop making it into something it's not.

Thank you.
