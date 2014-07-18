---
title: Fedora Jenkins-Fedmsg Enablement
tags: fedora
---

After a bunch of annoyances in dealing with Java version incompatibilities, I
was finally able to get
[fedmsg-java](https://github.com/fedora-infra/fedmsg-java) and
[fedmsg-jenkins-emit](https://github.com/fedora-infra/jenkins-fedmsg-emit) to
compile and work nicely together.

A lot of the issues dealt with the fact that our Jenkins master runs on Java
1.6, but I was compiling the plugin locally on Java 1.7. I then ran into the
fact that some of the dependencies I was using don't seem to have builds up on
Central for 1.6 anymore, so I had to downgrade to old versions.

All that being said, projects can now opt-in to have Fedmsg messages be sent on
completion of project builds and failures.

[An example message can be found here.](https://apps.fedoraproject.org/datagrepper/id?id=2014-ddfe6822-1bbb-4f5e-90c6-053985e39b76&is_raw=true&size=extra-large)

## To opt in

- Go to [Jenkins](http://jenkins.cloud.fedoraproject.org/) and log in.
- Click on your project to go to the project's profile page
- Click "configure"
- Scroll to the bottom and drop down "Add post-build action"
    - Click "Send messages to Fedmsg"
- Click the "Apply" button

You are done.

## To opt out, after opting in

Simply remove the post-build step created above.

You can trigger things to happen when failures or successes occur. For example,
you could have a node listen for relevant messages, then get pull the build and
generate/upload documentation whenever it sees a success.

Have fun riding the fedmsg bus!
