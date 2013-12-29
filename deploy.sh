#!/usr/bin/env bash

runhaskell -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d site build && rsync -Havzre 'ssh -p222' --delete _site/ elrod.me:/srv/webmount/hakyll
