---
title: Fixing a broken Firefox after version 35
author: Ricky Elrod
date: January 31, 2015
tags: firefox, fedora, software
---

So as of the last batch of updates I applied to my laptop (running Fedora 21),
Firefox started acting really weirdly. In particular, I was seeing the following
things:

- Even though my preferences were set to restore tabs when firefox started, I
  would only get a blank page when it started. No tabs would restore.
- I couldn't control-shift-t to open a previously closed tab. (I never realized
  how often I actually rely on this).
- I couldn't select "Restore previous session" or "Previously closed tabs" or
  "Previously closed windows" from the *History* menu.
- Password autofill didn't work. Not that I rely on this too much, but for sites
  I don't care too much about, I use it sometimes, and it was annoying that it
  stopped working.
- Maybe unrelated, but all JS on twitter wouldn't work, so I couldn't
  favorite/RT/tweet/etc.

I did a lot of searching around. I came across some older posts on
support.mozilla.org, but none of the suggestions there were helping (and most of
them were discouraged by Mozilla folks anyway).

After a lot more searching, I found
[Debian bug #775645](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=775645).

It described many of the issues I was having and a few more. Bingo!

It mentions that something set the `dom.indexeddb.enabled` preference to `false`
and that setting it back to true reverted all functionality back to normal. And
so it did.

I'm writing this post in the hope that anyone who uses Firefox and runs into a
similar issue finds it useful. I'm not sure which plugin specifically set
`dom.indexeddb.enabled` to false -- it wasn't anything I set manually.

Hopefully this is helpful to someone.
