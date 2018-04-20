---
title: Fedora Infrastructure Hackfest 2018 Recap
author: Ricky Elrod
date: Fri, 20 Apr 2018 10:35:40 -0400
tags: fedora, travel
---

Last week was the
[2018 Fedora Infrastructure Hackfest](https://fedoraproject.org/wiki/Infrastructure_Hackathon_2018)
which took place in Fredericksburg, Virginia. I think everyone who attended
found it to be a very productive week of hacking.

We started the hackfest off by brainstorming new documentation for packagers and
those interested in becoming a packager. We decided to use asciidoc and put the
documentation in a new
[git repo](https://pagure.io/fedora-docs/package-maintainer-docs) on
Pagure. Right now it uses Pelican and Asciidoctor for generating html versions
of the source files, but there was talk about eventually transitioning to
[Antora](https://antora.org/) at some point. The point of the new documentation
is that many of our workflows have changed with the move of packages/dist-git
into Pagure. For example, it's now possible to send pull-requests on packages
and ask maintainers to merge them. So the documentation needed to be updated to
reflect all of these changes, and it seemed like an ideal time to revamp and
rework the documents.

We decommissioned darkserver and summershum, two apps that had been
nigh-neglected for quite some time.

We worked on rawhide gating -- that is, making it so that Rawhide updates must
past through Bodhi. The big idea here is that we want to be able to let
automated tests run over Rawhide updates before they are pushed out to Rawhide
users, in order to make the Rawhide experience more stable. While Randy and
Patrick did most of the Bodhi updates to make this happen, I began working on
changes to the `bodhi` CLI. I am currently waiting until they have something for
me to test my changes against.

On the same day that was going on, I also deployed the redirect to move the
Fedora Jenkins setup into the CentOS infrastructure. It is now available
[here](https://jenkins-fedora-infra.apps.ci.centos.org/).

I also worked out a bug with the modernpaste paste-deactivation script I wrote
for us to deactivate pastes when necessary. Recently, I redeployed modernpaste
to Fedora 27 VMs, but the newer Python version caused an issue with the imports
I used. Ultimately, I had to add a missing import and all was good again.

We talked about setting up [Ansible AWX](https://github.com/ansible/awx/) and
came up with a plan and tried deploying it on a VM. I did most of the grunt work
of setting up the VM, and Kevin and I worked together on the initial AWX
attempts. Unfortunately, we ran into a fair number of what seem to be upstream
bugs. In the interim, in an effort to at least get _something_ up, we changed
our deploy (e.g. by making it use a local database container instead of our
external database VM), and eventually got it functional enough to play around
with. Patrick started working on integrating OpenID Connect authentication into
it, with the goal of users being able to log into our AWX with their FAS
credentials. AWX seems like it would be nice in that we'd have more fine-grained
control over which playbooks can be run by whom, however the pain of setting it
up so far has been fairly off-putting. It will be interesting to play around
with for a while longer and see what the team thinks.

My last day at the hackfest, Thursday, was discussion mainly about our OpenShift
instances. We discussed moving more apps into OpenShift, plans to upgrade our
instances to the latest releases, and so on. I signed on to move two more apps
into OpenShift in the near future. Patrick gave us a demo of how it looks to use
our current Ansible setup to deploy an OpenShift app. Over Thursday and Friday,
he and Randy worked on moving Bodhi to OpenShift, but ultimately, due to several
bugs in the Bodhi release, earlier this week, we reverted back to the old setup
for the time being.

This is what I worked on. There were some other things going on as well. RHEL
7.5 is now synced in our repositories and ready for us to upgrade to it after
freeze, along with OpenShift 3.9. Kevin worked on new priorities and templates
for our
[Infrastructure ticket tracker](https://pagure.io/fedora-infrastructure/issues/).

Oh, and on Thursday night, I sang two karaoke songs, winning a $25 giftcard to
the venue we were at for dinner/drinks that night. I ended up giving it to one
of the other singers, since I don't live in Virginia, and will likely never be
back at the venue.
