---
title: "An open letter to YSU IT: Please stop requiring password changes!"
author: Ricky Elrod
date: Wed, 26 Jul 2017 14:45:24 -0400
tags: ysu, it, security
draft: true
---

As a student member of the Security Policy Advisory Committee (SPAC) for
Youngstown State University, I feel it is my duty to bring forth issues that I
see with security policies with regard to campus networks and systems. In this
letter, I bring forth one such issue and I ask the rest of SPAC and YSU IT to
take it to strong consideration.

In September of 2014, YSU Information Technology Services (ITS) began to enforce
a policy which stated that student and faculty account passwords would be
expired every 180 days. These passwords control access to critical services such
as class registration, e-mail (via Office 365 SSO), Blackboard (via Blackboard
SSO), computer logins on campus (LDAP), campus Wi-Fi, VPN access (Cisco
AnyConnect), and many others.

If one of these passwords were to get compromised, the attacker could drop the
victim from classes, see their private email, access illegal content on their
behalf, hurt their grades by submitting assignments that haven't been completed,
and more. Therefore, the security of these passwords is an absolutely essential
part of a student's experience on campus, and quite literally in some cases,
their life and future.

Early on in my YSU experience, I was quite impressed that password changes
weren't required -- not from a user-experience perspective, but from a security
one. I would like to quote several well-respected sources to explain why
password changes **work against** the goal of creating a more secure
authentication.

[Troy Hunt](https://en.wikipedia.org/wiki/Troy_Hunt)
([https://www.troyhunt.com/](https://www.troyhunt.com/)) is a well-known
security expert who was named a Microsoft MVP in Developer Security (2011), and
a Microsoft Regional Directory (2016). On 26 July 2017, Hunt wrote
[an article](https://www.troyhunt.com/passwords-evolved-authentication-guidance-for-the-modern-era/)
on his blog about password security. In this post, in addition to providing what
appear to be an excellent set of password security guidelines, he also cites his
sources, some of which I will be referring to here.

Various large companies and governments are now either not requiring or actively
discouraging the use of periodic password changes/expiry. The NCSC, a government
agency in the United Kingdom,
[says](https://www.ncsc.gov.uk/guidance/password-guidance-simplifying-your-approach):

> Most administrators will force users to change their password at regular
> intervals, typically every 30, 60 or 90 days. This imposes burdens on the user
> (who is likely to choose new passwords that are only minor variations of the
> old) and carries no real benefits as stolen passwords are generally exploited
> immediately.

The organization goes on to say:

> Regular password changing harms rather than improves security, so avoid
> placing this burden on users.

The Microsoft Identity Protection Team's 2016
[paper on password guidance](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/06/Microsoft_Password_Guidance-1.pdf)
calls (pg. 9) password expiry for users an *anti-pattern* and states:

> Password expiration policies do more harm than good, because these policies
> drive users to very predictable passwords composed of sequential words and
> numbers which are closely related to each other (that is, the next password can
> be predicted based on the previous password). Password change offers no
> containment benefits; cyber criminals almost always use credentials as soon as
> they compromise them. 

This paper cites a
[survey](https://www.ftc.gov/news-events/blogs/techftc/2016/03/time-rethink-mandatory-password-changes)
by the Federal Trade Commission, in addition to a
[study](https://www.cs.unc.edu/~reiter/papers/2010/CCS.pdf) by the University of
North Carolina (which states in its abstract that "We believe our study calls
into question the merit of continuing the practice of password expiration.") and
another [study](http://discovery.ucl.ac.uk/20247/2/CACM%20FINAL.pdf) by
University College London (which claims that practices such as
password expiration end up being less secure due to users writing down
passwords, as a result of not being able to memorize new ones easily).

Carleton University released a
[study](http://people.scs.carleton.ca/~paulv/papers/expiration-authorcopy.pdf)
in 2015 which states:

> In this note, we quantify the security advantage of a password expiration
> policy, finding that the optimal benefit is relatively minor at best, and
> questionable in light of overall costs.

Even the National Institute of Standards and Technology (NIST), a
government organization which recently announced their
["Special Publication 800-63"](https://www.nist.gov/itl/tig/special-publication-800-63-3)
suite which "provides technical requirements for federal agencies implementing
digital identity services," has dropped their requirement for password expiry,
as a result of the research.

It is my hope that the sources noted above present a case that requiring
passwords to be changed after a certain time period actively compromises the
security of student and faculty accounts on campus for (at least) the following
reasons:

- Account holders are more apt to choose insecure, easy-to-remember passwords,
  since they know they will go away in 180 days anyway.
- Account holders are more likely to write down their passwords, since they need
  to remember a new one every 180 days.
- Account holders are likely to simply increment a number or add a single
  character to their existing passwords. There exists password-cracking software
  based around this concept. When a user changes their password by choice, they
  are likely to choose a unique, fresh password. When they are forced to change
  it every 180 days, they are likely to keep incrementing a number and fall
  susceptible to password-cracking software which expects them to do so.

With all of this published research stating that password expiry actively
**decreases** security, for the reasons listed immediately above, **I implore
the YSU ITS to please reconsider and revert this policy.**

Additionally, the original 2014 email sent out to faculty and students mentioned
that "[this] effort was undertaken to meet a myriad of federal, state, and
university auditor requirements." None of these audits were mentioned by
name. However, I would like to ask YSU ITS to consider whether these audits are
a good use of university funding, given that they are actively pushing for the
university to employ practices which compromise its security. Please take this
into consideration as well.

Sincerely,
A security-minded student.
