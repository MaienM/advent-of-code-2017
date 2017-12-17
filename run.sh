#!/bin/bash

set -o errexit -o pipefail

clear

# Build file
ghc --make AOC$1.hs -main-is AOC$1

# Cleanup build files
find . -type f -regextype posix-extended -regex '.*[.](dyn_)?(hi|o)' -delete

# Run tests, if any
if [ -f AOC$1Spec.hs ]; then
   runhaskell AOC$1Spec.hs
fi

# Run input file through the executable
cat AOC$1.txt | ./AOC$1
