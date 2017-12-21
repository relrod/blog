#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

rm -rf "$cwd/_site/" "$cwd/_cache/"

if [ ! -f "$cwd/dist/build/blog/blog" ]; then
  echo "blog binary not found."
  exit 1
fi

"$cwd/dist/build/blog/blog" clean
"$cwd/dist/build/blog/blog" watch
