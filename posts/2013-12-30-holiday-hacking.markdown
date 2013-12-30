---
title: Holiday Hacking
tags: fedora, blog
---

This post is inspired by a similar one written by
[nirik](http://www.scrye.com/wordpress/nirik/2013/12/28/holiday-geeking-so-far/).

I think this is one of the more productive holiday breaks I've has in the past
few years. I've used the time to completely go to town on some of my personal
projects.

- The first day or two of break I just took it easy. I've recently become
  addicted to watching
  [tournament videos](https://www.youtube.com/playlist?list=PL8702A0B148E81034)
  by Jerry/ChessNetwork, so a lot of the time was spent eating pizza and
  watching those.
- One of my side projects, [http://eval.so/](eval.so) was in major need of a
  rewrite. Originally written in Scala using the Play framework, I decided a
  while back that I wanted to redo it in Haskell. It is comprised of two parts:
    - The frontend is what provides the actual JSON API to users. Users can
      submit code to it and it will ask the backend to evaluate it in a sandbox
      before sending the result back to the user.
    - The backend is what actually calls out to the SELinux `sandbox` command.
      Currently it is only used by the frontend code, but it could, in theory,
      be used by any kind of frontend (think Thrift/RPC, etc.) with not too
      much pain.

  So my first big project was to dive into this Haskell rewrite head-first. I
  started a Cabal project for the backend a few weeks ago, but hadn't really
  had the time to sit down and work on it, and this was my chance. The backend
  rewrite, aptly named "Cruncher" because it crunches numbers/code into a
  result, is now [on GitHub](https://github.com/eval-so/cruncher). I am still
  porting over the language definitions from the old system, although this is
  a pretty easy (though boring :P) process.

  The Frontend was a little more interesting. I had to pick a Haskell web
  framework. There are a decent number to choose from, each with their own
  pros and cons. I played with Yesod for a bit, and I particularly like how it
  handles static assets. It allows you to do page-specific assets (CSS/JS) very
  easily (it will cram everything a page needs into one file for the browser to
  request), and handles caching/etag for you out of the box. It is able to
  provide a large amount of compile-time safety in the template system, which
  is really neat. For example, instead of:

```html
<img src="/static/img/doge.png" />
```

  ...you would write:

```html
<img src=@{StaticR img_doge_png} />
```

  This prevents you from ever referencing a file that was moved/deleted. In the
  above example, if I move `img/doge.png` to `img/such_meme_wow/doge.png`
  without updating the `img` tag accordingly, this will result in a compile-time
  error. This doesn't just work for static assets, but for all handlers.

  However, this safety comes at a bit of a cost. Yesod has a
  [bit](http://stackoverflow.com/questions/5645168/comparing-haskells-snap-and-yesod-web-frameworks)
  of a [name](https://github.com/yesodweb/yesod/wiki/Too-much-Template-Haskell%3F)
  for using a [lot](http://stackoverflow.com/questions/11884455/how-to-learn-a-new-library-framework-in-haskell/11885197#11885197)
  of Template Haskell, which arguably makes it seem very
  [magical](http://www.djangopony.com/). I like fully understanding the
  tools that I use, or at least being able to if I choose. I am not implying
  that it's impossible to come to know Yesod, but based on IRC logs and blog
  posts around the internet, it can at least be a lot more challenging than
  some of the other Haskell frameworks.

  It was around this time that Ed Kmett happened to be talking in #scalaz on
  freenode about his experience with Haskell web frameworks. Based on the
  [amount of Haskell code](http://hackage.haskell.org/user/EdwardKmett) he
  [has written](https://www.quora.com/Reviews-of-Haskell/review/Edward-Kmett),
  and his (very) respectable level of involvement in the Haskell community, I
  really valued his opinion, and kept reading the conversation. (This is posted
  with his permission):

```irc
< edwardk> yesod has more magic, but [it] also has better "incorrectness caught at [compile-time]" properties
< edwardk> snap sites build way faster
< edwardk> and eventually you understand all the magic
< edwardk> whereas with yesod the magic never goes away fully
< edwardk> otoh, i like knowing more things are correct, so i'm torn
< edwardk> i like snap's design, and i like yesod's efforts to show your site is correctly linked
< edwardk> yesod has a bunch of stuff for setting up resources for your site as data types, so the links to those resources [typecheck]
```

  I did end up [choosing](https://github.com/eval-so/frontend3) Snap based on
  this dialog. So far, I have been really happy with it, but I haven't done
  anything extremely complex with it yet. The idea of composable components
  (Snaplets) is really appealing. It allows for a "do one thing and do it well"
  mentality. Then, once everything is working, you can compose them into a 
  single web app easily.

- While working on all of that, I realized that eval.so made a perfect usecase
  for the new Fedora Copr system. I already
  [blogged about that](http://elrod.me/posts/2013-12-28-fedora-coprs-and-eval-dot-so.html),
  so I will keep this one short.
- My friend Jim/KG4SGP came on IRC shorly after I posted that blog post, and
  me how my blog worked. I explained that it was Jekyll and that if I were to
  do it over again, I would choose Hakyll. But then I got thinking...
  "I have the time now, why *not* do it over again?"
  [And so it was](http://elrod.me/posts/2013-12-29-switching-to-hakyll.html).
- I set up a beta deploy of the Haskell rewrite of the eval.so stuff. I won't
  advertise the link to find it here because it is barely even polished enough
  to call beta, but if anyone is interested, it is not hard to find.
- I have been studying chess tactics. I don't know what my ELO is right now
  (other than completely terrible), but I think it would be fun to find out and
  set a goal to climb to over the next year.
- Random other small things as they arise. I filed a
  [bug](https://github.com/yesodweb/Shelly.hs/issues/47) on the Shelly library
  for Haskell; [tried](https://github.com/yesodweb/shakespeare/pull/118) adding
  [PureScript](https://github.com/paf31/purescript) support to Yesod's
  shakespeare-js library; edited a few Wikipedia articles

So basically: Lots of Haskell, lots of pizza, and chess here and there.

I will be going to visit some family from my dad's side in the first week of
January. I will be flying out to NYC on 1/4 and returning 1/8. I very much look
forward to visiting the city. I haven't been out there since elementary school
on a field trip.
