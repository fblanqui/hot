#!/bin/bash
set -x
set -e

for i in ghci cabal happy ; do 
  which $i > /dev/null
done

cabal update
cabal install haskell-src-exts