---
title: The ultimate emacs-based vocabulary-learning environment
author: Ricky Elrod
date: Fri, 04 Nov 2016 23:44:36 -0400
tags: emacs, fedora, latin, linguistics
---

Okay, okay. Maybe "ultimate" is a bit extreme. But seriously, I have yet to find
a better way to study vocabulary. Flash cards work well, but I wanted something
more. And perhaps in the future, I can easily generate flash cards using this
setup.

So, I'm teaching myself Latin. Or trying to. I'm using "Lingua Latina Per Se
Illustrata." And I'm throwing the vocabulary words at my face and studying the
crap out of them. Not just the words, but cognates/related words in English,
interesting etymology where it applies, etc.

I've been hanging out and idling in ##latin on freenode. User "Godmy" there has
talked in the past about his study habits for becoming proficient in Latin and
has shared spreadsheets that he has created for drilling the vocabulary. I liked
this approach, but modified it and actually started writing the vocabulary (and
related notes) in a physical notebook. But I thought I could do a bit
better. Godmy has also spoken about how having *quick* access to look up words
and etymology was vital to his studying process. I use Duck Duck Go, so I very
often type `!wt foo` or `!eo foo` for looking up words on wiktionary or
etymonline, respectively.

Earlier today I had a random thought, though. Similar to Godmy's spreadsheet
idea, what if I made an `org-mode` table for the vocabulary. I've used these for
things in the past and grew to love them. I can export them in a bunch of
formats, and manipulate them in a bunch of ways. Plus I get all the power of
emacs right at my fingertips as I do so.

I wanted quick access to wiktionary and etymonline, like Godmy suggested. So the
first thought I had in using Emacs for this was "I need a way to launch a web
browser with wiktionary (and/or etymonline) for the word under the cursor." I
am not an emacs lisp expert, so I did some searching around. Ultimately I came
up with the following function (based
on [this](http://ergoemacs.org/emacs/emacs_lookup_ref.html)):

```elisp
(defun lookup-wiktionary ()
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (de-latinize (replace-regexp-in-string " " "_" word)))
    (eww (concat "https://en.wiktionary.org/wiki/" word))))
```

I stuck that in `.emacs` and added a keybinding:

```elisp
(global-set-key (kbd "C-c w") 'lookup-wiktionary)
```

And then I did the same for etymonline:

```elisp
(defun lookup-etymonline ()
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (de-latinize (replace-regexp-in-string " " "%20" word)))
    (eww (concat "http://www.etymonline.com/index.php?allowed_in_frame=0&search=" word))))

(global-set-key (kbd "C-c e") 'lookup-etymonline)
```

A few things of note:

- These both use `eww` -- the built-in emacs web browser. This means no CSS, no
  JavaScript, etc. In other words, the lookups are *freaking fast*.
- The `de-latinize` function. Oh boy, the `de-latinize` function. Writing this
  caused me to realize I should really *actually* learn emacs lisp one day
  because this took me way longer to write than I am willing to admit. The idea
  is that I want to be able to look up latin words with macrons in them over the
  vowels, because both of these sites don't use the macrons in the URLs for
  words. So I needed to replace those vowels with non-vowel versions of the
  letters. This is ultimately what I came up with:

```elisp
(defun de-latinize (str)
  (let ((mapping '((?ū . ?u)
                   (?ā . ?a)
                   (?ē . ?e)
                   (?ī . ?i)
                   (?ō . ?o))))
    (mapcar (function (lambda (x)
                        (alist-get x mapping x 0)))
            str)))
```

...It's simple, but I am *completely* not used to emacs lisp syntax, so it took
me a while to write it. It simply maps over the given string (`str`), replacing
letters with those in the map if it can, otherwise leaving the letter alone if
it doesn't exist in the map.

So I was ready to get going. Except I needed to be able to easily *type* those
macrons, if I wanted to learn the correct pronunciation of these words. A quick
google showed me I can go to my org-mode buffer with my vocabulary table, and do
the following: `M-x set-input-method RET latin-alt-postfix RET`. Now I am able
to type vowels and hit `-` (dash) after them, to place a macron above them (for
example: `a-` will produce `ā`).

Here's a quick screenshot of the setup in action:

<img src="https://images.srv1.elrod.me/latin-vocab-org-mode-etc.png" />

I am convinced (for the time being, at least) that there is no better way to
study vocabulary. This setup is awesome.
