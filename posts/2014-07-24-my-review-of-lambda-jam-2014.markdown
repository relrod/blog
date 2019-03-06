---
title: My review of Lambda Jam 2014
author: Rick Elrod
date: July 25, 2014
tags: fedora,haskell,travel
---

I arrived home early this morning after spending Tuesday and Wednesday in
Chicago and Monday travelling to Chicago. Why was I in Chicago, you ask? For
[Lambda Jam](http://www.lambdajam.com/) (LJ, for short) of course!

This post aims to review my (really awesome and fun) Lambda Jam experience.

# Getting there

I got in on Monday night after driving for around 6 hours from Ohio. The drive
was uneventful, although tolls are extremely expensive and annoying.

After finally being able to find the hotel, I learned that they only have valet
parking, so I reluctantly let them park my car and got situated in my room. I
texted Tony Morris (dibblego) to see what people were up to and shortly after
I learned that Rob Norris (tpolecat) had just recently gotten in and all of us
headed to dinner at [Big Jones](http://bigjoneschicago.com/).

# Day 1

To open the conference, Rich Hickey gave a keynote talk on ways for programs to
interact with each other. Specifically, he introduced us to
[Transit](https://github.com/cognitect/transit-format), which describes itself
in its readme as "a format and set of libraries for conveying values between
applications written in different programming languages."

I then went to Gershom Bazerman (sclv)'s talk which introduced us to homotopy
type theory in a very non-scary way. It was really neat to see the concept
introduced in the way that Gershom did. I was able to understand pretty much all
of what he covered and it is definitely a very interesting area that I want to
explore a lot and learn more about.

I stuck around in the same room for the "One Weird Type" talk by Katherine Ye.
She introduced a fairly simple problem and showed us how to go about solving it
in a provable way in the language Coq using the induction principle.

Then I went off to Brian McKenna (puffnfresh)'s talk about Idris, where he
demonstrated his (really, *really* cool)
[Iridium](https://github.com/puffnfresh/iridium) project, which is basically
Xmonad in Idris and with the X11 binding abstracted away so that the core works
with Quartz/OS X, X11, and other display servers, just by writing a very small
binding library. It's really freakin' cool, go check it out.

I finished off the day by
[making a programming language](https://github.com/CodeBlock/-) - a simply typed
lambda calculator - in Rúnar Bjarnason (runarorama)'s workshop. Okay, fine,
there's no typer yet, but the evaluator works. I added a simple parser the next
day. I plan on adding a simple typer to it soon.

We went out to eat at the
[Public Chicago Hotel](http://www.publichotels.com/chicago/home/). A bit
expensive compared to what I'm used to, but very tasty.

# Day 2

The opening keynote of day 2 was given by Erik Meijer. It was all about the
ways in which Visual Basic is better than Scala.

I kid. Kind of. ;)

It was actually a very good talk that had a lot of really good quotes to leave
with (a small sampling are included in my list below). The main idea that I left
with was that programmers should embrace math because without laws our data
structures and their so-called properties are just buzzwords.

Next, Ed Kmett gave a talk about a really cool way to derive a key-value
structure similar in nature to Haskell's `Data.Map` but which has an insert
speed that is several times faster, at the cost of slightly slower lookups.
I plan on revisiting this after I read Okasaki and seeing what I can do with it.

I stayed in the room for the next talk which was about Scalaz, by Tony Morris.
He walked through some of the history of scalaz and some of the compromises that
the Scalaz team refused to take because they broke the principles on which
Scalaz was started. Strict adherence to the core principles (parametricity,
equational reasoning, and abstraction) shaped Scalaz into the library it is now.

Next I went to the Property-based testing talk by Jessica Kerr (jessitron). I
already knew how to use quickcheck/scalacheck (and if *you* don't, you should!),
but it was helpful to see the style of presentation used, since I'll be giving
a talk about similar tools (more generally, on the problems with standard unit
testing) at [Flock](http://flocktofedora.org/).

After that, I just took the "hallway track" and hung out with Tony, Brian,
Rúnar, Stew, and some others, and talked while we each worked on various
projects. I was working on implementing the parser for the lambda calculator I
made in Rúnar's talk from day 1.

A small group of us went to dinner at
[The Purple Pig](http://thepurplepigchicago.com/). The way it works is that you
order food for the table and all share it. I'm a rather picky eater and so I
didn't try everything that everyone else did, but it was fun. If nothing else, I
learned that I like port wine. I'm not much of a drinker, so I'm never sure
what to order when I order drinks, but that was a good choice. While we were
there, I hounded Ed with a bunch of category theory questions. I was mostly
interested in how he came to learn it so well, because I wish to do so as well.
That is an ongoing process - although learning it as well as he knows it will
*always* be an ongoing process. ;-)

# Overall

I had an **excellent** time and I very, very, very strongly recommend that if
you have any interest at all in functional programming, you make it to the next
Lambda Jam.

The organization of the conference was great - there was never a time-slot that
had no interesting talk to go to, and there were never two talks simultaneously
scheduled of which I both wanted to see really badly. Huge hat-tip to Dave
Thomas and the rest of the LJ team!

# A few quotes I liked from LJ

- "Real power comes from restriction." -Ed Kmett
- "If you can play a Mario game, you can do mathematical proofs." -Erik Meijer
- "Visual Basic has better type inference than Scala!" -Erik Mejier
- "I'm old, I'm bald, I grew up while Dijkstra was alive. He's turning in his grave when you say 'objects request…'" -Erik Meijer
- "'I use F#, I'm a functional programmer,' is like saying, 'Q'doba is Mexican food.'" -Erik Meijer
- "If I didn't like being told that I'm wrong, I wouldn't be programming Haskell." -Anonymous (someone tweeted this as overheard, but there was no attribution.)
