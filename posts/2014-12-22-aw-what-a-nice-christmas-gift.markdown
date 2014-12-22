---
title: Aw, what a nice Christmas gift!
author: Ricky Elrod
date: December 22, 2014
tags: da.gd, statistics, attack
---

It seems that over the last few days http://da.gd/ (a URL shortener that I made
and run) fell victim to a botnet attack.

I was casually talking with some friends on IRC and writing some Haskell code
when I got a weird e-mail from DigitalOcean (where da.gd is hosted). It said
simply "Oh no! We've found an issue with your account and issued you a new
ticket that needs to be addressed as soon as possible."

I assumed this was no good, otherwise they probably would've given some more
detail. I fought with my phone -- which had randomly turned off for some
reason -- and finally opened FreeOTP to get a two-factor code and log into the
DigitalOcean panel to read the ticket.

It appeared to be an automated ticket with no human intervention at all. In
fact, other than the title, the ticket didn't mention abuse at all. But it did
show a lot of e-mail headers and have a da.gd link mentioned a few times.

I clicked on the da.gd link and found that it went to a pornography/advertising
site. Now, running a URL shortener (especially one without any kind of captcha),
I expect to get some amount of spam. It's something I've accepted, and I don't
vet the short URLs because in general, it's none of my business what URLs users
of da.gd want to shorten.

I responded to the ticket and explained that while it contained a da.gd URL,
da.gd didn't create the spam. It was just a tool used to create links that were
put in the spam message.

However, after responding, I thought it would be a good idea to just do some
sanity checks and make sure everything was ok. I looked at the database and was
astounded at what I saw: Today alone there were over 1.5k new short URLs
created. I knew something was wrong.

I didn't know immediately what the average was for how many new short URLs come
in each day (now I know it's about 26), but I knew it wasn't anywhere near 1k.
So I began digging.

I won't list the actual sites here, but I saw many, many short URLs created
pointing to pornographic sites, advertising sites, and a few random unrelated
sites (but still in high enough numbers that it was obviously spam). But the IPs
of many of the short URL creations were different.

I didn't want to spend too much time playing with data right then, because I
tend to like to take my time and analyze data like this fairly carefully. The
more important thing to do was to 1) stop these new URLs (which were still
coming in at several per second) and 2) disable all of the URLs which matched
the target of the spam so that whoever was getting the emails containing these
links wouldn't click and get redirected to some pornography site they had no
intention of going to.

So I did that. I quickly
[patched](https://github.com/relrod/dagd/commit/8132b530f45c42544ab019522bb207fc6f6eb236)
the code to add regex-based blacklisting of so-called "long URLs." Luckily, the
URLs that were being added en masse seemed to be in groups which had common
parts of the URL. e.g. one set might have been "http://foo.bar/?randomString"
and another set might have been "http://asdf.qwerty/?randomRandom" and both sets
had many entries en masse with only the querystring changing. So the blacklist
patch was enough to immediately stop the ongoing attack. I deployed it, added
the regex patterns to the production config, and immediately stopped seeing new
spam entries added to the database. Phew.

At some point during all of this, I started watching the access log for the
VirtualHost. It was flying by really quickly, many requests per second. All of
the new attempts at adding URLs were resulting in HTTP 400 responses now, but
I was still seeing a lot of HTTP 200 responses, which seemed to be going to the
short URLs. "Oh dear, well-meaning people are going to pornography sites,
possibly loaded with viruses."

So I put a stop to that (the actual patterns have been replaced with
`[REDACTED]` below):

```
MariaDB [dagd]> update shorturls set enabled=0 where longurl like '%[REDACTED]%';
Query OK, 17630 rows affected (0.35 sec)
Rows matched: 17630  Changed: 17630  Warnings: 0
```

Whoa. Almost 18k. This had to have been happening for a while, right? Better
disable the other sets too...

```
MariaDB [dagd]> update shorturls set enabled=0 where longurl like '%[REDACTED]%';
Query OK, 16557 rows affected (0.35 sec)
Rows matched: 16557  Changed: 16557  Warnings: 0
```

Oh dear.

Luckily, the logs started filling up with 404s now (so people who would have
been getting sent to these spam URLs were simply getting a 404 error) and 400s
now (so the, I assume, botnet which was adding all of these URLs wasn't getting
anywhere because the blacklist was blocking them).

And slowly the 400s started dying out. I assume the botnet clients didn't know
how to handle an HTTP 400 response and crashed or something.

Now that it was under control, I could do some basic playing-around-with-data.

I exported the dates of all short URL creations to a file and copied it to my
laptop. I tried opening it in libreoffice-calc, which worked fine until I tried
to graph it. It turns out it doesn't like graphing nearly 85,000 data points. Oh
well. I wrote a quick Haskell program using the awesome
[Diagrams](https://hackage.haskell.org/package/diagrams) and
[Chart-diagrams](https://hackage.haskell.org/package/Chart-diagrams) libraries
and everything was fine.

Here's the extent of the attacks.

### da.gd short URL creations (Mid October - December 2014)

<img src="http://images.srv1.elrod.me/dagd_attack_oct-through-dec.svg" />

### da.gd short URL creations (2011 - December 2014)

<img src="http://images.srv1.elrod.me/dagd_attack_2011-through-2014.svg" />

Not pretty. :-(

### da.gd attack coming to an end

<img src="http://images.srv1.elrod.me/dagd_attack_stopping.svg" />

This graph deserves some explaination. We see tonight's go at the (multi-day)
attack start up around 12:38AM (Eastern time) (which agrees with the logfiles).
You can see us serving a ton of HTTP 200 responses. At about 12:45, I deploy the
blacklist fix (patch linked above), and we immediately start seeing 400s.
Success!

You can see how the 400s fade away for a while before completely stopping. My
only assumption here is that whatever was spamming da.gd couldn't handle the
400s and crashed on however many boxes it was running on.

Then you see the 404s, likely from people clicking the spam links in their
e-mail messages. Another success!

### Wrap up

In summary, playing with data is fun, but stopping attacks isn't. Please don't
attack da.gd or use it to attack others. It's quite mean of you and it isn't in
the holiday spirit at all. :-(.

Importantly, though: Any e-mails you may have received over the past few days
containing da.gd links **did not originate from da.gd**. da.gd was just the URL
shortener the spammers used to mask the link they wanted you to click. Don't
blame da.gd, pretty please.
