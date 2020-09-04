---
title: How Ansible/ansible-base releases work
author: Rick Elrod
date: Thu, 03 Sep 2020 20:30:13 -0500
tags: ansible,development,release engineering,fedora
---

Back in March of this year, I transferred teams within Red Hat and joined the
Ansible Core team to work primarily as a release engineer/release
manager. Shortly after joining, I shadowed how releases were cut, and from there
began to cut releases myself.

I've had a number of people including some friends ask how we handle our
branching and release schedule and cut releases, so I wanted to address that and
discuss it some.

In what follows, I use the term "Ansible" to refer to both: `ansible < 2.10` and
`ansible-base >= 2.10`. "Ansible" -- the community distribution of collections
-- follows its own development cycle, independent of `ansible-base`.

Keep in mind that this process does change over time, so this post is likely to
become outdated as time goes on. As a recent example, when I started, we no
longer published release candidates for every patch release[^1], and I have
pushed to start doing them again.

[^1]: These were done up until around Ansible 2.5 or so and then phased out.

Let me start with defining a few terms.

| Term          | Definition                                 | Examples                                       |
|---------------|--------------------------------------------|------------------------------------------------|
| x-release     | A major release                            | 1.0.0, 2.0.0, 3.0.0                            |
| y-release     | A minor release                            | 1.1.0, 2.1.0, 3.2.0                            |
| z-release     | A patch release                            | 1.1.1, 2.1.4, 3.2.9                            |
| z-stream      | The x-y series that a z-release falls into | 1.2.3 and 1.2.4 are in the same (1.2) z-stream |
| stable branch | Git branches beginning with `stable-`      | stable-2.10, stable-2.9                        |

## Releases

At any given time, 3 z-streams of Ansible are supported. Currently, the
supported z-streams are `2.8`, `2.9`, and `2.10`. This means that we must
release bugfix and/or security updates for each of these streams and keep them
tested and working. **We do not backport features into already-existing
z-streams**, only bugfixes and security updates.

By
[policy](https://docs.ansible.com/ansible/devel/reference_appendices/release_and_maintenance.html),
the older a release, the less updates it receives. Currently, this means that
2.10 gets the most updates -- it gets all bugfixes and security updates; 2.9
gets most bugfixes as well, and all security updates; 2.8 only gets security
updates and not much else.

## Branches

In [ansible/ansible](https://github.com/ansible/ansible) (which is the home of
`ansible-base >= 2.10` as well as `Ansible < 2.10`), we use the `devel` branch
as a continuous development stream, and `stable-*` branches for work that
corresponds to existing releases of Ansible or ansible-base (one for each z-stream). We have
documentation
[published](https://docs.ansible.com/ansible/latest/community/development_process.html#backporting-merged-prs)
which explains how the backport process works.

In short, nearly all features, bugfixes, and security patches happen in the
constantly-moving `devel` branch. Bugfixes and security patches can, in most
cases, be backported to the stable branches and land in the next z-release for
each one (so long as they comply with the policy linked above).

## Schedule

For a long time, z-releases have come out approximately every 3 weeks. With the
[reintroduction of release candidates](https://groups.google.com/forum/?oldui=1#!topic/ansible-announce/cFA8XBYSFSs)
(discussed below), we have moved to a 4 week cycle for stable releases: A
release candidate is published, then one week later a stable release is
published. Three weeks after that, the process repeats with a new release
candidate.

<img src="/static/ansible-release-cycle.png" alt="Ansible release cycle" />

### Release Candidates

Ansible has always published release candidates for new x-releases and
y-releases, and until circa version 2.5, published release candidates for
z-releases as well.

When I came onboard and took over release engineering duties, I pushed for
starting release candidates for z-releases again. This was largely due to some
regressions in published z-releases that should have been caught much earlier
but weren't, along with some pressure from teams both internally and externally
for more opportunities to test a release before it is published. The lesson and
mantra was that **we have people willing to help us test early, let us help them
help us.**

### Stable Release

One week after a release candidate is published, a stable release is published
(possibly containing fixes to any regressions found in the release
candidate). We do not normally currently publish multiple release candidates for
a given z-release, as this would shift the rest of the schedule in an annoying
way. We are also **more apt to revert a backport than try to fix it at the last
minute** after a release candidate has gone out. Rather than rushing a fix, it
is often better to revert the change, and ask the developer to fix it for the
next release candidate in 3 weeks.

This is also why it is critical to keep releases flowing and make sure the cycle
stays consistent -- putting out a new release candidate 3 weeks after a stable
release has gone out. We want to be able to say "Sorry, we had to revert this
backport because it broke something. But that's no big deal! There's a new
release candidate coming out in 3 weeks, just get it fixed up for that!"

## Testing

There are tests that get run throughout all of the process. `ansible-test` is a
powerful tool for testing both Ansible and collections, and the Ansible
repository uses it in combination with an extensive suite of unit and
integration tests.

Tests are run on every pull request into devel and every backport pull
request. Heuristics are used to determine which tests to run, based on which
file has changed. A *full* run of the test suite happens both nightly and as
part of the actual z-release process itself, where we require (as part of our
checklist) that tests are green before continuing.

In addition to this release and pre-release testing, the reintroduction of
release candidates has given teams (internally and externally) a week to test
each z-release and report issues to us before the stable version comes out.

Lastly, I have created an independent project,
[aut](https://github.com/relrod/aut) which uses Travis CI and Docker to test our
published artifacts and make sure they landed where they were supposed to and
work well enough to run a simple "hello world" playbook. The details are
available in the project repository, but effectively, it just creates a build
matrix where it will test our releases.ansible.com artifacts, our PPA artifacts,
our pypi artifacts, and so on. It is meant to act as a post-release test that
simply serves to help me sleep better after a release, knowing that the
artifacts at least run.

## The Process

The actual details of the release process are
[published](https://github.com/ansible/community/wiki/RelEng:-ReleaseProcess) as
a (slightly outdated but mostly accurate) checklist.

Most of the actual release work -- distributing the artifacts to where they need
to go -- is done through an internal Jenkins instance. Currently the build
scripts that Jenkins uses are not published, but I have plans to clean them up
and publish them on GitHub soon.

# Closing Thoughts

I find that release engineering is an exciting role to be in. While it comes
with a lot of trust and responsibility, it also comes with an enormous amount of
opportunity for impacting change.

Release engineers/managers live in an integral part of the project, coordinating
the structure between developers, testers (or those willing to play that role),
and users, and balancing processes to make each group happy. There is much that
can be said about how to best manage and achieve that goal:

* How do you stay out of the way of people developing new features or reworking
  complex parts of the codebase, while still getting the changes into the hands
  of people who can test it in time?
* How do you gate what should go into a backport versus the next y-release of
  the project?
* How do you constantly test your build engine and know when changes to the
  configuration will break it?[^2]
* How do you know your release artifacts actually worked?

[^2]: This is something else that I recognized immediately when I first
    started - we currently have no staging environment for our Jenkins build
    setup. This is something I am planning to work towards very soon.

These are the kinds of questions that I keep in mind as part of my primary role
on the team, and although I am learning the codebase itself and enjoy hacking on
it, these kinds of release engineering questions are at the forefront of my mind
at all times.

If you have questions about our process or want to chat with me or discuss
ideas, I am always available on IRC. Feel free to ping me (`relrod`) in
`#ansible-devel` on Freenode!
