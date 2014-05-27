#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
"$cwd/dist/build/blog/blog" build && rsync -Havzre 'ssh -p222' --delete _site/ elrod.me:/srv/webmount/hakyll
