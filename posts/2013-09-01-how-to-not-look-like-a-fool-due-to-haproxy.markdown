---
title: How to not look like a fool due to haproxy
comments: true
tags: fedora, infrastructure
---

Today I deployed
[mw-FedoraBadges](https://github.com/CodeBlock/mw-FedoraBadges) to production
in Fedora Infrastructure. It had been in staging for several days now, and we
wanted to get it live before the freeze.

As I deployed it, a few interesting things happened.

First off, it's useful to know that we use haproxy in front of our app servers.
Haproxy basically creates a round-robin of app servers for each application
that it sits in front of, and internally routes requests to random servers (or
servers based on a set of criteria).

Today I went to move the mw-FedoraBadges extension out of staging and into
production. When I did this, I moved all of the files that I modified from our
`modules-staging/mediawiki` directory into `modules/mediawiki` - or so I
thought.

It turns out that I missed one file - the module manifest, which had an
important change in it: it added the `mediawiki-FedoraBadges` package as a
required package on machines which include the `mediawiki` module. The other
files (a small CSS tweak, and a much more important one-line change to the
wiki's `LocalSettings.php` config file, which included the extension) all
deployed just fine. But since `LocalSettings.php` had updated and the servers
knew nothing of the package that `LocalSettings.php` was requiring, things
started to 500.

Luckily, when I make big changes like this, I only trigger a run of Puppet on a
very small subset of our app servers -- in this case, one server. As soon as it
updated all files but didn't show any sign of installing a new package, I knew
what I had done.

I quickly checked the haproxy status page of one of our proxies (it didn't
matter which, since they largely share the same configuration), to make sure
that it had pulled the app server out of its internal round-robin.

I saw this and instantly breathed a sigh of relief:

<img src="https://images.srv1.elrod.me/i-love-haproxy-so-much.png"
     alt="haproxy saves the day" />

I added the package requirement to the Puppet manifest and re-ran Puppet on
app01. A few seconds and a haproxy-status-page refresh later, all was green
again.

At this point, I figured that everything was fine. app01 seemed to be doing
fine and serving traffic, so I ran Puppet on the rest of the servers. Ut oh.

I waited a while for Puppet to run (I was using Ansible on our config
management server, lockbox01 to trigger the Puppet run on appXX). After a
while of getting impatient, I refreshed the haproxy status page again, and saw
that app01 was just fine, but all the other app servers had gone red in the
`fp-wiki` section. Oh crap.

I flipped back to the terminal, and saw that the Puppet runs had finally
finished. `mediawiki-FedoraBadges` 404'd when Puppet tried to install it via
yum. What the heck?

I looked closer and saw that it was actually trying to install the old version
of `mw-FedoraBadges` - the one that had been in staging, which was a slightly
different version number than the one I was deploying. Yum had cached the old
version number, when it had last read the repo data of our infrastructure repo
and Puppet had failed to `expire-cache` to ensure the latest version was being
obtained.

I used Ansible to `yum clean expire-cache` on all app servers, re-ran Puppet on
them, and everything returned to green instantly.

We never hit outage, but for about 1-2 minutes our wiki was running on one app
server. Haproxy had saved me from outage not once today, but twice. In about
five minutes. I now love haproxy. :)

In slightly less stressful news, now you can show off your Fedora Badges on
your user page in the Fedora Wiki.

Simply add:

`{{ #fedorabadges: your_fas_username }}`

where you want them to appear, and

`{{ #fedorabadgescount: your_fas_username }}`

anywhere you want a count of your badges to appear.

You might need to add `?action=purge` to your userpage the first time, if
anything goes weird with caching. The extension leverages caching because of
the fact that it has to call out to Tahrir (which has the Badges information)
to get your list of badges. This call is potentially really slow, and caching
makes it be slow once, instead of on every page load.

The cache should last ~24 hours I believe, so at most, someone's badges, as
displayed on their user page, will be 23 hours and 59 minutes out of date.
You can always purge the cache by adding `?action=purge` to the URL of the
user page.

As an alternative (but hacky) way around this, I'm thinking about having the
badge awarder purge the cache of the awardee's user page, when they are
awarded a badge. It can do this asynchronously and best effort with no real
downside if it fails, because it's pretty low priority. I have no idea if this
is something worth pursuing, but it sounds hacky.
