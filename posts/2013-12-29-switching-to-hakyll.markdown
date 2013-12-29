---
title: Switching to Hakyll
tags: haskell, blog
---

After thinking about it for some time, I finally made the switch to Hakyll. I've
been learning Haskell for the past several months, and after getting annoyed
with how often Jekyll and Octopress broke (every time I tried to update them),
this seemed like a logical move.

Moving things over to [Hakyll](http://jaspervdj.be/hakyll/) wasn't really all
that hard, since it has support for the awesome
[Pandoc](http://johnmacfarlane.net/pandoc/) library. This meant that all of my
markdown from Jekyll would easily port over (with a few changes to the metadata
fields, which were accomplished with
[codemod](https://github.com/facebook/codemod)).

Overall, I really like Hakyll so far. My
[site.hs](https://github.com/CodeBlock/blog/blob/master/site.hs) file is barely
over 100 lines and for the time being does everything I want. Hakyll even
handles [tags](http://jaspervdj.be/hakyll/reference/Hakyll-Web-Tags.html) and
[pagination](http://jaspervdj.be/hakyll/reference/Hakyll-Web-Paginate.html),
and can handle
[calling out](http://jaspervdj.be/hakyll/reference/Hakyll-Core-UnixFilter.html)
to other system utilties, process assets such as
[sass/scss](http://sass-lang.com/) or [Roy](http://roy.brianmckenna.org/)
scripts.

The last thing I wanted to do before calling my migration done was set up some
redirects. In the past, I had two Jekyll instances, one for my blog, and one
for my personal site. With this migration, I merged them into one Hakyll site.
So I needed to redirect old links from "blog.elrod.me" to "elrod.me".

Hakyll, by default, exports things with slightly different filenames than
Jekyll/Octopress. Instead of "/blog/2013/12/29/post-name/index.html", Hakyll
uses "/posts/2013-12-29-post-name.html".

Both of these are very similar, and doing a simple rewrite was all I needed.
So I created a new VirtualHost on my server, for the old subdomain, and came up
with this:

```apache
<VirtualHost *:80>
  ServerName blog.elrod.me
  RewriteEngine on
  RewriteRule ^/blog/(\d+)/(\d+)/(\d+)/(.+)/$ http://elrod.me/posts/$1-$2-$3-$4.html [R]
  RewriteRule ^/blog/(\d+)/(\d+)/(\d+)/(.+)$ http://elrod.me/posts/$1-$2-$3-$4.html [R]
  Redirect / http://elrod.me/
</VirtualHost>
```

...which doesn't cover every page on the old site, but at least redirects the
old blog post URLs.
