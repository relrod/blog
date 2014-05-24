---
title: I love being wrong
date: 2014-05-24
tags: logic, learning
---

I have held on to this post for a little over a month now, and I still think it
is worth sharing, so I am going to post it.

When I first started programming in 2005 or so, I constantly leaned on people
to show me how to do things. Being young, it was harder to wrap my head around
the logical concepts that now seem obvious and are absolutely essential to being
a productive software engineer. But people were usually patient and willing to
help.

As I got more and more adept to the basic concepts of programming, I learned
some very important things, not just about programming, but also about the
social details that come along with it.

In 2010, I joined the Fedora project, for example, and was very welcomed by the
community. This gave me first-hand insight into the amount of time and
collaborative skills necessary to not only be a part of, but help to
technologically run such a community (because I joined the infrastructure team).
For the first time, I got to see how a sysadmin team of multiple people actually
works. I got to see for myself the costs and benefits of such a team. For
example, a cost is that sometimes people attempt to fix the same problem and end
up stepping on each others' toes by accident. A benefit is that when fixing
problems, there are a lot of very smart people around, and if I run into
trouble, I can just ask one of them, and they will likely be able to help or
point me in the right direction.

While I was gaining this insight into the community, I decided to join another
at-the-time aspiring (and now very successful) open source community: the
Phabricator project, lead by Evan Priestley. I contributed a number of patches
(82 to date - I am the #4 top contributor to the project, according to GitHub)
and constantly took note of how Evan was running the project. I was inspired by
his humor and wit, and how much freedom he gave me to send patches and ideas.

Almost every time I had an idea, he would offer advice on how to generalize it
and improve it. It was very rare that he would resist one of my ideas, and when
he did, it was always because he had already thought of either a better way to
do it, or had already weighed the pros and cons of the idea, and concluded that
the implementation would hurt the project more than it would help.

But along with being accepting of new ideas, he also showed me something,
probably without intending to: He showed me that it is okay (and good) to be
wrong.

Every time somebody came in #phabricator and brought up an issue, Evan would do
one of a few things:

- Immediately solve the issue by sending the user a patch, and asking him or her
  to test it
- File a bug and say "It'll be fixed after <another bug> is fixed"
- Say "It's already been fixed, upgrade your install"
- Say "Well, that is intended behavior, but you can do what you want by doing X"
- Say "Oops, we should document that better, let me push a patch."

He would never pin blame on anyone, especially the person who brought up the
issue. That was very inspirational to me. He very obviously cared about the
project and wanted to improve it. He never carried a "I know better than you"
attitude, and always assumed that there was a problem on the project's end
before anything else. He was so good at this, that I almost got scared to ask
questions, because I didn't want him to invest a ton of time on something silly
that I was just overlooking. In fact, this lead me to get more familiar with the
codebase, because I wanted to triple-check that it was, in fact, a bug in the
code, before I accidentally wasted his time.

More recently (over the past year and a half), I have gotten very interested in
functional programming. Because of the vast paradigm shift compared to the
object-oriented Ruby/Python/Perl/PHP programming I had always done, learning
functional programming required me to basically forget everything I had learned
about programming and start fresh.

Along the way, I got to know a number of people in the Scala and Haskell
community. In particular, the #scalaz channel on Freenode, has been one of the
most helpful channels in persuing my goal of properly learning functional
programming. The regulars in #scalaz carried a similar attitude of "it is okay
to be wrong - it is a chance to learn." In particular, Tony Morris often says
that not knowing something (or being wrong about something) is never the
problem, "it is what happens next that matters." I love this line of thought
because it is encouraging.

He explained this in more detail to me privately a few weeks ago, when he and I
were discussing a research paper idea. I began writing a first draft of the
paper and admitted that I was nervous for him to read it, because in a way, I
look up to him and didn't want him to think less of me if I wrote something
silly without intending to. He quickly calmed my nerves by saying that the only
way he would think less of me is if I reacted negatively to him telling me that
I wrote something bad.

I explained that I shared his attitude of using being wrong as a chance to learn
and to improve, and I explained that if he pointed out that I was wrong about
something, I would simply ask for his advice on how to correctly relearn what I
misunderstood.

---

Lately, I have encountered/interacted with a number of people who have grown
very attached to certain ways of thinking, certain projects, and certain
beliefs. I am not out to tell people to stop believing things that I personally
think are illogical or silly; it isn't my place in the world to do so. However,
I think there is much merit to being willing to step back and look if someone
points out something to you.

Instead of choosing to be blinded by what is familiar, it can be useful to step
back, think about what the person is saying, and use logic to re-formulate your
thought. Even if you still disagree, the exercise can be useful.

I think sometimes people get so attached to how they do things, that any
criticism, no matter how constructive, is either shrugged off or taken
offensively, instead of as a chance for potential learning, and I think this can
be very harmful.

I love being wrong. I embrace it. When I am wrong, and someone can prove to me
that I am wrong, it is a chance to learn - and I love learning.

Be open to being wrong once in a while. It does you a lot of good, and gives you
much more credibility in the world. Don't be afraid to not know something.

```
"Science works on the frontier of knowledge and ignorance. We're not afraid to
admit what we don't know. There's no shame in that. The only shame is to pretend
that we have all the answers."
```
  - Neil deGrasse Tyson
