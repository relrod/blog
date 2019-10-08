#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
#"$cwd/dist/build/blog/blog" clean && "$cwd/dist/build/blog/blog" build && rsync -Havzre 'ssh' --delete _site/ relrod@hosted.elrod.me:/srv/www/elrod.me/webroot
cabal run blog clean && cabal run blog build && rsync -Havzre 'ssh' --delete _site/ relrod@hosted.elrod.me:/srv/www/elrod.me/webroot
