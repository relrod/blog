---
title: Adding a Fedora Project edge proxy back into DNS rotation
author: Ricky Elrod
date: Wed, 05 Jul 2017 00:07:32 -0400
tags: fedora, infrastructure, open source
---

I had to pull one of the proxies (proxy10.phx2.fedoraproject.org) from the
fedoraproject.org roundrobin rotation the other day for some maintenance. One of
our log rotation configurations wasn't working right, so the disk had filled
up. As a result, things couldn't write to disk, I/O got backed up, and the load
went up a fair bit:

```
[codeblock@proxy10 ~][PROD]$ w
06:35:29 up 1 day,  8:20,  1 user,  load average: 880.60, 884.96, 886.51
```

Once I pulled the proxy and dealt with the issue, I thought it would be
interesting to see how fast our DNS pushes actually get distributed out. So I
took a video using `recordmydesktop` of me adding the proxy back into DNS
rotation. You can see how quickly one of the Apache logs starts logging hits.
The IPs in the logs have been masked out in the video.

The embed here starts at 120s, which is right after I actually run the command
to push out the DNS change to add the proxy back in.

<iframe width="854"
        height="480"
        src="https://www.youtube.com/embed/_xXu4VqBL8Y?t=120"
        frameborder="0"
        allowfullscreen></iframe>

You can see that traffic starts hitting the proxy within *seconds*.

I just thought it was interesting to see and thought others might get a kick out
of it.
