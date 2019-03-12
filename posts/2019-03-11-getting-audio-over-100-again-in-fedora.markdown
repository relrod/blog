---
title: Getting audio over 100% again in Fedora
author: Rick Elrod
date: Mon, 11 Mar 2019 22:29:32 -0400
tags: fedora
---

A few weeks ago, I built a new PC for myself and have since been using it for
both work and leisure. I run a dual-boot on it of Fedora 29 and Windows 10.

When I set up Fedora, I decided to try something different and went for the XFCE
spin. It worked well, but due to a somewhat complex monitor setup (two screens
and a TV that I want to use sometimes as a third monitor) where each screen
needs to have separate scaling/DPI, I resorted back to using Gnome on Wayland so
that I can control the scale of each monitor individually. This might have been
possible on XFCE, but mucking around with `xrandr` seemed more annoying than it
was worth.

However when I installed Gnome, I noticed something: Previously, going to
settings -> sound would allow you to raise the global system volume over
100%. Now it wasn't letting me.

I don't know if this is because I installed Gnome later (and thus didn't get
some default that is set on the "normal" Fedora Workstation ISO) or if the lack
of this ability happened as a result of newer Gnome than I had used in the past.

In any case, I did some searching around and source code hunting discovered
there's a handy little `gsettings` toggle that can be flipped to re-allow that
behavior.

```
[rick@sapphire ~]$ gsettings list-keys org.gnome.desktop.sound
input-feedback-sounds
theme-name
allow-volume-above-100-percent
event-sounds
```

Promising.

```
[rick@sapphire ~]$ gsettings get org.gnome.desktop.sound allow-volume-above-100-percent
false
```

Ah ha!

```
[rick@sapphire ~]$ gsettings set org.gnome.desktop.sound allow-volume-above-100-percent true
[rick@sapphire ~]$ gsettings get org.gnome.desktop.sound allow-volume-above-100-percent
true
```

And by the time I had flipped back to the Settings window, the audio boost was
there again and I could slide it over beyond 100%.

Hope this helps someone.