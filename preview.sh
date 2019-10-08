#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

rm -rf "$cwd/_site/" "$cwd/_cache/"

cabal run blog clean
cabal run -- blog watch --host 0.0.0.0
