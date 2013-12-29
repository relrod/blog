---
title: Fedora Copr and eval.so
tags: eval.so, fedora
---

One of my side projects, http://eval.so/ (which I will be writing more about in
the near future), depends on a lot of programming language environments. In
short, it provides a sandboxed JSON API to let people evaluate code in a lot of
languages.

This isn't any kind of official launch/announcement for the project (in fact,
it is being rewritten and the current API is going to change very soon) - I
just wanted to introduce it enough for context of the rest of this post.

Because of the large number of languages that eval.so tries to support, it is
true that not every compiler/interpreter we want to support is officially
packaged in our distro of choice for running the system (Fedora). For example,
eval.so supports [Rust](http://rust-lang.org/), but Rust isn't
[yet](https://bugzilla.redhat.com/show_bug.cgi?id=915043) packaged for Fedora.
We also support some [sillier](https://github.com/justinmeza/lci) languages
that nobody has bothered to package yet.

For development purposes, we have been using a
[custom repository](https://github.com/eval-so/yumrepo) with some
[hacky scripts](https://github.com/eval-so/yumrepo/blob/master/specs/build.sh)
for maintaining the packages within. This has worked well, but has ended up
becoming something of a time-sink as the scripts need to be updated for each
new Fedora release, and all the packages have to be rebuilt (usually on my
laptop). Essentially we were using a few hacky bash scripts as a build system
before uploading the RPMs and repo data up to S3.

I knew this was the wrong approach, but it worked, and it seemed easier than
setting up a huge build system like [Koji](http://koji.fedoraproject.org) for
our custom packages. But it came to my attention that there exists another
solution that didn't involve setting up Koji or using hacky shell scripts.

Enter [Copr](http://copr.fedoraproject.org/).

Copr is a Fedora-hosted system that is similar to ArchLinux's AUR or Ubuntu's
PPA system in that it allows people to create unofficial repositories
containing unofficial packages, so long as those packages are legal and
acceptable by the
[guidelines](https://fedorahosted.org/copr/wiki/UserDocs#WhatIcanbuildinCopr).

Our usecase is perfectly in-line with what the
[original idea](https://fedoraproject.org/wiki/Category:Copr?rd=Category:Kopers#Usage_Cases)
for creating Copr was. So by using Copr we no longer need to maintain hacky
shell scripts and S3 repos. We simply make .src.rpms of our custom packages,
upload them somewhere, then tell Copr to go out and build them and add them to
our repository. It handles the rest.

There's also a really cool [API](http://copr.fedoraproject.org/api/), so we
could, in theory, script builds to occur as other events happen. For example,
we could relatively easily[1] have a new RPM build occur with each commit to
a repository.

Another cool thing about Copr is the CLI application (which uses the API I
mentioned above). You don't even need to leave your terminal to trigger builds.
Pierre-Yves (pingou) speaks about this CLI
[here](http://blog.pingoured.fr/index.php?post/2013/03/04/Fedora-Infra%3A-Did-you-know-copr-cli).

One last thing - It is worth noting that I plan on eventually trying to get
the packages in our
[new eval.so copr](http://copr.fedoraproject.org/coprs/codeblock/evalso/) into
Fedora's official repos. But it's a lot easier to test things and get them live
quickly by putting them in a custom repository before having to go through the
review process required for official packages.

So bye-bye http://yum.eval.so/ and hello Copr!

[1] This would be slightly complicated by needing something listening for a
post-receive hook from GitHub and actually triggering the build, but this
could be done very easily via Travis-CI by storing the Copr API key in a
[secret environment variable](http://about.travis-ci.org/docs/user/encryption-keys/).
