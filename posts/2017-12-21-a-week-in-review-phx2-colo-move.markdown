---
title: "A week in review: PHX2 Colo Move"
author: Rick Elrod
date: Thu, 21 Dec 2017 00:11:42 -0500
tags: fedora, sysadmin
---

A few weeks ago (4 December 2017 - 9 December 2017), I had the opportunity to
fly to Phoenix to help out with a move of Fedora servers in our main colo site
(phx2). This post outlines some of my experiences.

I flew in Sunday (3 December 2017) and got to the hotel. It was around 1pm and I
had nothing else to do that day, so I took an uber to the
[Musical Instrument Museum](https://mim.org/), which was amazing, but I'll save
that for another post.

On Monday, I was waiting for my coworker to arrive, so I called in some
hard-drive replacements from the comfort of my hotel room, and ended up heading
over to the datacenter and having some of the on-site techs let me in and show
me around a bit. This was my first time in a production datacenter that wasn't
part of a university, so it was neat to see.

I had to confirm that I was who I said I was, but after a while, I was given a
visitor badge, which allowed me access throughout the building and into the
colo cage.

There was a drive that had already been called in and needed to be replaced, so
I took care of that while I was there. Other than that, I just hung out, talked
with the on-site techs a bit, and looked around.

Tuesday through Friday is a bit of a blur because of how many things were
happening. Everyone was focusing on their own projects and tasks, so I'll talk
about some of mine:

* Playing with console servers (OpenGear)
    * Updating firmware
    * Labeling which ports were going where
    * Testing to make sure the console cables all worked
* Taking inventory
    * Making a spreadsheet of each rack, which server was where, which PDU ports
      it belonged to, etc.
    * Crash-carting servers to see what they were, when nobody knew.
    * Physically labeling servers which were not already labeled.
    * Checking wiring, making sure dual-PSU boxes weren't both going into the same
      PDU, and so on.
* Updating aarch64 boxes like crazy
* Replacing a bunch of HDDs with SSDs in one of our ARM servers.
* Various other things that came up (playing with networking wires for various
  PPC boxes, etc.)

Most of that is pretty straightforward, but takes a long time. The console
server stuff involved finding where to download the firmware, tracing console
wires, and whining at people for changing console wires (or adding new ones)
without telling me so I could update my documents.

The inventory-taking took much longer and involved a lot of crash-carting,
playing with label machines, and walking around with a tablet, updating a
spreadsheet of what was where.

I did come up with a clever idea to make inventory easier going forward: Using
Ansible variables to generate the documents we were automatically coming up
with.

For example, we keep a document that is an ASCII representation of our racks and
the servers in them and how many units they take each up. My idea is to put all
of that information in Ansible variables and generate that document
automatically...

The result would be that our physical boxes have something like:

```yaml
rack_units: 2
rack_location: 10
rack_number: 147
```

... in their Ansible host-variables files, and there would be a script to
generate a pictorial representation of our server racks, based on this. We could
do similar for PDU ports, console ports, etc.

The idea is that we already use Ansible for storing information about our
inventory anyway, so why not make it smarter and put everything in one place?
I will write another post about that, showing off the script, once I actually
add those variables and write the script.

Anyway, this isn't meant to be a status report on the trip, but to show some of
what I've been busy with.
