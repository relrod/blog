---
title: Life full-time at Red Hat
author: Rick Elrod
date: Mon, 03 Jul 2017 21:51:41 -0400
tags: fedora, red hat, linux
---

On 09 May 2017, I transitioned from being a long-term intern at Red Hat, to
being a full-time member of the Fedora Engineering team. In this post, I'll
discuss a bit about what I've been working on and how the transition worked.

## Background

I had been talking with my manager, Paul Frields, for some time about
transitioning to full-time after college. Long story short, the timing so
happened to work out that I could be brought on slightly before I'm officially
done with college. To that end, I am planning to finish college out part-time
from here on out. I still have to take an Ethics course to finish my computer
science degree, and I still have some math classes left, for my math degree. I
plan on going <= 6 credit hours per semester until I am done, however long that
takes.

## The Transition, New Hire Orientation, and the Infra Team Hackfest

When I was brought onboard as an intern back in 2012, I opted out of going to
New Hire Orientation (NHO), in the interest of not hvaing to miss
classes. However, since the transition to full-time was after the semester,
going to NHO was feasible. Paul did some work to make it so that the week of my
NHO, a two day series of presentations, was during the same week as the
[CI and Infrastructure Hackathon](https://fedoraproject.org/wiki/CI_and_Infrastructure_Hackathon_2017)
which I also attended that week. On Monday and Tuesday (the days of NHO), I went
to the hackathon after the NHO presentations were over, to join up with the
team. After Tuesday, I stayed with the team for the rest of the week and stayed
involved in the hackfest. During the hackfest, I was also given my first
official full-time task: Go into the iDRAC of all of our R520 servers, and check
for failed drives. I did so, but realizing that this is actually a common task,
I made a note to automate this later on, and a few weeks later wrote a script to
log into the iDRAC, and check the drive status automatically. I'll probably
write another post about that at some point.

Since I didn't write at all about the hackfest before now, I'll write briefly
about it here. We got a lot done, and ended up getting an OpenShift instance in
our staging environment. More information on that can be found on the
[Fedora Wiki](https://fedoraproject.org/wiki/Infrastructure/OpenShift) and the
[Fedora Gobby instance](https://fedoraproject.org/wiki/Gobby) which has a
document on it. I also got to talk with
[Patrick Uiterwijk](https://patrick.uiterwijk.org/) who briefed me on some of
the things within our Open*Stack* cloud deployment, since that is something I
will likely be working on down the road. In between various meetings and talking
with people, I was setting up my new loaner laptop with a fresh Fedora 25
install, and getting things roughly configured on it.

## What I've Been Up To (in Fedora)

As with any sysadmin job, a lot of my time is spent fixing random breakage that
comes up. However, some of the things I've been up to that aren't that:

* We did updates/reboots of all of our servers before the Fedora 26 Final
  infrastructure freeze.
* Patrick recently showed me the process to do updates on the OpenStack cloud,
  which went surprisngly smoothly.
* I've been working on an `fpaste` rewrite. I'll have another post on this
  soon. I'm doing it in Haskell for a number of reasons and have been working
  with the Haskell SIG. The repo is [here](https://github.com/relrod/fpasteng),
  but the name will be changed at some point.
* The iDRAC script mentioned above.
* Learning about OpenShift, and looking at getting some of our smaller apps
  running on it for testing purposes.
* Looking at getting
  [S3 mirroring](https://pagure.io/fedora-infrastructure/issue/6022) back up for
  Fedora repos.
* Learning how to call in bad hard drives for RMAs/replacements.
* A lot of other things that I'm forgetting.

## Five Years or Nine Weeks?

<img src="https://images.srv1.elrod.me/5-year-puck.jpg"
	 alt="5-year service award puck" />

I've been a full-time employee for about nine weeks now. Yet, I recently
received my five year service award puck for being with Red Hat for five
years. This is because of the long-term internship that I had before I became
full-time. Nevertheless, I love the service award puck!

## Going Forward

I'll be trying to write more posts about things I'm working on (both in Fedora
and not), going forward. Stay tuned!
