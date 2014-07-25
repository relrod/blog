#!/usr/bin/env bash

if [ "$1" == "" ]; then
  echo "Usage: new_blog_post.sh 'Title of the Post'"
  exit 1
fi

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

# Taken from https://gist.github.com/saml/4674977
title="$1"
max_length="${2:-48}"
slug="$({
    tr '[:upper:]' '[:lower:]' | tr -cs '[:alnum:]' '-'
} <<< "$title")"
slug="${slug##-}"
slug="${slug%%-}"
slug="${slug:0:$max_length}"

date="$(date '+%Y-%m-%d')"
file="$cwd/posts/$date-$slug.markdown"

longdate="$(date '+%B %d, %Y')"

if [ -f "$file" ]; then
  echo "A post with that title already exists from today's date."
  exit 1
fi

echo "---"                  > "$file"
echo "title: $title"       >> "$file"
echo "author: Ricky Elrod" >> "$file"
echo "date: $longdate"     >> "$file"
echo "tags: "              >> "$file"
echo "---"                 >> "$file"
echo                       >> "$file"
echo "Created: $file"
echo "Done."
