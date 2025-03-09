+++
title = "Becoming multilingual in German and Rust"
description = "Using Rust to automate the import of media into the language-learning platform LingQ."
date = 2025-03-07

[taxonomies]
tags = ["rust", "german", "open-source"]
+++

# Background

I enjoy learning new technologies and programming paradigms and languages. I
always have. And it's something that I've had less time to do over the past
several years.

In 2020, I switched roles away from my position on the Fedora Infrastructure
team and into a more developer-oriented role on the Ansible team. When I did
that, something unfortunate that happened without me even realizing until much
later was that I committed less time to my side projects. You could almost say
that I burned out: I was writing code for work, so I felt less need to
experiment with it off the clock.

In practice, this had some unfortunate ramifications. Above all, it meant that I
fell a bit behind on new languages and stacks that weren't used in my dayjob
work. And now it's my goal to back-fill some of that knowledge and experience.

In late 2023, I moved to Germany, and learning German has been a process for me,
ever since. Before my move, I learned about the online reading platform
[LingQ](https://lingq.com) and began using it rather extensively.

## What is LingQ?

LingQ is a platform for language-learning that lets you import books, articles,
and other kinds of media to read. It will track which words you've seen before,
which words you know, and how well you know them (self-graded). As you come
across new words, it will show definitions that other users have added for that
word, or allow you to look up the definition and add your own.

Although it has its fair share of issues, outages, and downtimes, which I could
talk about at length, I've nevertheless found it the best platform for this sort
of thing, and I have to give it credit: My German would not be where it is right
now, without having used it.

# Automating Importing Content into LingQ

## My first attempt

Somewhere along the way, I developed a fairly extensive Python toolset for
importing content into LingQ. I never open-sourced it, for a number of reasons
-- not the least of which, it was hacked together and never meant to become the
monster it became. The toolset handled a number of cases:

- First, I wanted to be able to import "syndicated content" automatically into
  LingQ. Things like podcasts, YouTube videos from creators that I enjoy, even
  daily news articles from sites like [Nachrichten
  Leicht](https://www.nachrichtenleicht.de).
- I also wanted to be able to import random articles or videos I found. (LingQ
  has a browser extension for doing this; I wanted something more general that I
  could use for local content, too).

Over time, this Python toolset I built up became fairly heavy-handed. It did a
*lot*. And over all, *it worked well, but it be became harder to maintain*. The
usecases and scope grew over time.

For example: When you import audio content into LingQ, it can use AI to
transcribe the content to generate LingQ lessons. Pretty neat, and overall, it
almost works well, but it doesn't make any effort to split up the text into
paragraphs based on who was speaking, which was something that annoyed me. So my
Python toolchain grew the ability to send audio content to
[OpenAI's Whisper API](https://platform.openai.com/docs/guides/speech-to-text),
take that response, feed it to a
[text generation model](https://platform.openai.com/docs/guides/text-generation)
to massage it a bit to make it more readable instead of just being a giant text
blob, and upload *that* transcript instead.

Most of the media downloading was ultimately powered by the amazing
[yt-dlp](https://github.com/yt-dlp/yt-dlp) project. When you have a link to some
media (be it YouTube, an mp3, or some video on some news side), there's a good
chance that yt-dlp will know how to grab that media.

The Python code was fine. It ran in GitHub Actions for a long time,
[on a
schedule](https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows#schedule).
But because it grew in scope so much, it became hard to maintain, and it was
time to redesign it.

So. I had a new side project. And by now, I had realized and acknowledged that I
needed to learn some new tools and start branching out again instead of just
focusing on the dayjob stack. So?

# Cut-to: Rust

I made the decision to start learning the Rust language. Rust is extremely
interesting to me as someone who enjoys delving into type systems and
programming language design. Rust solves entire classes of memory-related bugs
due to its type and lifetime systems. I believe it has a lot of really neat
ideas plus a very active community. I've tinkered with it before, but never to
any serious degree, and it was time.

Ultimately, what I wanted first and foremost was a system that would let me
specify different "media feeds" and import the latest content from them into
LingQ, in whatever way made sense for that specific feed. LingQ is broken into
specific "courses" each with an ID, so each feed will import "lessons" into a
specific course.

LingQ has [a page](https://www.lingq.com/en/accounts/apikey/) that will generate
an account API key, but the API is largely undocumented. Understanding the API
took a fair bit of reverse engineering, which is perhaps best saved for another
post. But suffice it to say, between examining what the site itself does when a
lesson is created (by watching the browser inspector), and decompressing the
"official" LingQ browser extension, I was able to understand the endpoints
enough to do what I needed to do, which was just to import lessons and get a
list of existing lessons.

Here's what I wanted at a high level:

{% mermaid() %}
graph LR
    A[lqcli]

    subgraph "GET Syndication Feeds"
    B[Podcast ...]
    C[YouTuber ...]
    end

    A --> B
    A --> C

    F[Process each feed] --> G[Upload new episodes as lessons to LingQ if necessary]

    B --> F
    C --> F
{% end %}

# Feed Processing

What does processing each feed look like? Well, we know that we don't want to
upload a lesson if we've already uploaded one. We could keep track of that state
locally somehow, but I wanted this thing to work in Github Actions (or any other
kind of automation pipeline) and not have to worry about that. Besides, LingQ
knows which lessons it has. So we can just ask it. We can get a list of lessons
(episodes, in our case) in each course, and just compare the names. Sure, if an
episode gets renamed, it'll get uploaded again -- but in practice this is very
rare.

Now, for some sources, we can rely on the built-in LingQ transcription, as
mentioned earlier. But for some, we want to run it through Whisper ourselves, so
that we can do some post-processing on it to clean it up a bit before we turn it
into a LingQ lesson. The feed processing ends up looking like this:

- Query LingQ for existing lessons in the relevant course for the feed
- Parse the latest N entries (episodes) from the feed
- Compare those N entries against the latest N lessons in the LingQ course
- If we find a match, we're done, move to the next episode
- If not, then we need to import a lesson:
  - Download the media (usually via `yt-dlp`, but other "fetchers" could be
    added to the code and referenced in config)
  - (Optionally) Send the audio to OpenAI to get a transcript
    - Perhaps in the future, be able to run Whisper locally
  - (If we did transcript in the last step) post-process the transcription to
    make it look pretty
  - Take the audio, and the transcript if we have one, and upload it as a
    lesson.

Well, that's relatively easy enough for the common case. There are other cases
that this doesn't handle, that the Python code _does_ already handle. For
example, the amazing [Easy German](https://www.easygerman.org) Podcast publishes
transcripts for members -- and these transcripts are much better than anything
Whisper can come up with; they are already pre-tagged with the speaker and
everything! So being able to download the archive that includes those
transcripts and pull out the appropriate one is kind of an edge-case that would
be nice to have. Not necessary, but something the Python code does and that
would be good to port to the Rust version in the future.

# Where `lqcli` stands right now

All of this is still very much a work in progress. The code is
[on GitHub](https://github.com/relrod/lqcli), but is not nearly complete yet. In
fact, being one of my first ever Rust projects, it's quite messy, and I find
myself refactoring it each time I work on it. But this is part of the fun, for
me, and most importantly, part of the learning process.

Let's talk about what currently works.

#### Config file parsing

It will look for a config file in `~/.lqcli.toml` where you can define your
LingQ API key, your OpenAI API key, and your sources (feeds) for importing and
monitoring.

Here's a brief view of what my config file looks like:

```toml,name=~/.lqcli.toml
[lingq]
api_key = "..."

[openai]
api_key = "..."

[[sources]]
download_method = "yt-dlp"
name = "14 Minuten"
url = "https://anchor.fm/s/f60c4864/podcast/rss"
course_id = 1760029
language = "de"
tags = ["daily"]
transcript_via = "openai"

[[sources]]
url = "https://www.youtube.com/feeds/videos.xml?playlist_id=PL3936178A38BB5F87"
course_id = 1328855
language = "de"
tags = ["daily"]
name = "Easy German"

[[sources]]
url = "https://www.youtube.com/feeds/videos.xml?playlist_id=PLk1fjOl39-53GxQIn1Hxdouokf0J0SDpl"
course_id = 1328855
language = "de"
name = "Super Easy German"
tags = ["daily"]

# ...
```

The first source I list (the awesome "[14 Minuten](https://14minuten.de)"
podcast for German learners) is a somewhat "full" example. The _default_
(and indeed, _only_, at this point in time) `download_method` is `yt-dlp` so
specifying it is unnecessary, but it's there as an example. Similar for
`transcript_via`, the tool will transcribe media (using OpenAI) by default.

The `tags` feature is a feature that I added fairly early on - it does not exist
in the Python version, and it's something I wanted. The main purpose is that I
want to be able to specify _when_ certain feeds get checked. Let's say that I
run `lqcli` in automation every 6 hours. There is no point in checking a feed
that I _know_ will only release content at _most_ once per day. So having the
feeds tagged, means I can schedule how `lqcli` is run (in automation) and have
one automation that runs daily (passing `--tags daily` to only check those), and
another automation that runs every 6 hours (passing `--tags 6hours` or whatever
I choose to call the tag).

#### Argument and Subcommand Parsing

Using [clap](https://docs.rs/clap/latest/clap/) to parse subcommands and
arguments was downright *fun*. What a cool crate! It provides not only a
builder-style DSL for creating command argument parsers, but also a way to
simply specify the command hierarchy as logical types (structs and enums) and
derive traits and use macros to configure the behavior of the parser right
in-line with the type definition. The doc comments become the help text for the
commands and flags.

In `lqcli` I opted for the derive method and my code ended up looking like this:

```rust
#[derive(Parser, Debug)]
#[command(version, styles = styles())]
/// Command-line interface to import content into language-learning platforms
/// such as LingQ.
struct Cli {
    /// Path to the configuration file to create or read from
    #[arg(short, long, default_value = "~/.lqcli.toml")]
    config_file: String,

    /// The category of action to perform
    #[command(subcommand)]
    subcommand: MainSubcommand,
}

#[derive(Debug, Subcommand)]
enum MainSubcommand {
    /// Import content from periodicals such as podcasts or YouTube channels
    #[command(subcommand)]
    Sources(SourcesSubcommand),

    /// Transcribe a single piece of content
    Transcribe(TranscribeSubcommand),

    /// Import a single piece of content
    Adhoc(AdhocSubcommand),
}

#[derive(Args, Debug)]
struct TranscribeSubcommand {
    /// The URL of the content
    url: String,
    /// The language code of the content
    language: String,
    /// How to download the content. Usually the default of "yt-dlp" is fine.
    #[arg(long, short = 'm', default_value = "yt-dlp")]
    download_method: fetch::DownloadMethod,
}

// ...
```

There is more to it than this, but this gives you an idea of how `clap`
works. The `clap` documentation (linked above) is amazing and they have plenty
of examples in their source repository.

Using `clap` also gave us an awesome `-h/--help` system for free.

#### Checking LingQ and Asking for Lessons

The next bit that already works is using the
[reqwest](https://docs.rs/reqwest/latest/reqwest/) crate to query LingQ and ask
for a list of lessons in a given course (by using the course ID number).

#### Creating a LingQ Lesson In a Course

Extending on the above, it was easy enough to also add a method for creating a
lesson in LingQ. I made a light abstraction over
[`reqwest::Client`](https://docs.rs/reqwest/latest/reqwest/struct.Client.html)
so that we could create a client and store that in some struct instance and
re-use it. It also handles things like setting up the required `Authorization`
header for authenticating to the LingQ API.

#### Getting a Transcript from OpenAI Whisper and Tweaking It

Using [async-openai](https://docs.rs/async-openai/latest/async_openai/), I am
able to send audio data to OpenAI and get a transcript back in German. I am
then able to send it *back* to OpenAI and ask it to format it in a reasonable
way.

With these bits alone, I was able to implement a few subcommands:

#### `lqcli transcribe`

This is a command I added primarily to test the OpenAI transcription
functionality, but have found it useful enough to leave it as a command. It
takes a URL to some media file, downloads it (using `yt-dlp` by default,
overridable with an argument), then sends the downloaded audio data to OpenAI to
transcribe it and post-process it before dumping out the transcription to
standard out. It's basic, but it works. Currently, there's no way to override
the post-process prompt in this command.

#### `lqcli sources sync [-t TAG]` (partially)

This command is what will actually do the work of importing episodes that need
to be imported into LingQ. Right now, it is capable of telling you which
episodes need to be imported, but it doesn't actually do the full workflow
yet. It's close, though. Filtering by tag (`-t`) does work.

#### `lqcli adhoc` (partially)

This command imports an arbitrary piece of media at some URL, into LingQ. It
doesn't currently handle local media (this is planned) - it expects media to
come from a URL. You pass in the URL to the media, a title, the language code
and the LingQ course ID, and it will do the rest: Download, transcribe,
post-process the transcription, upload everything to LingQ. Optionally pass `-s`
to skip transcription and let LingQ transcribe the content itself.

# Thoughts and What I've Learned

So there is still some work to do to finish up this project. It's rather niche,
so I'm not sure anyone other than me will find it useful, but it's been a great
project to learn Rust. I got hands on experience not only with Rust itself, but
also with some really popular and cool libraries, like `clap` and `reqwest`, and
it's conducive to two of my current goals: learning Rust and learning German.

There are also some possible places to take this project to learn some more. For
example, we could make requests concurrently, getting to play more with Rust's
`async`/`await` functionality.

But for now, it gave me a cool project to use to start learning Rust. And I'm
glad for that: I'm really, really enjoying Rust so far.

I've learned quite a bit about Rust through this project, though it's really
only scratching the surface and I'm excited to learn much more. In addition to
the more uninteresting bits (like getting more familiar with the syntax of
Rust), I got to learn about borrowing and (indirectly) about lifetimes. I got to
learn about the different representations of strings (which I had learned about
in the [Rust book](https://doc.rust-lang.org/book/) but, being a hands-on
learner, it didn't really click until I got to play with them first-hand.

I got to learn about project structure in Rust, how modules work. I got to learn
about formatting and linting tools like `rustfmt` and `clippy` and about how to
get a good development flow going for Rust in emacs. And of course, I got to
learn how much I still have to learn.

Largely, I just got (ever slightly) more comfortable with Rust and its ecosystem
and wrote some real code for a real project, because that's how I learn
best. And it was fun.
