#!/bin/sh

# Run tests
if [ -f test/$1Spec.hs ]; then
   cabal exec runhaskell -- -isrc -itest test/$1Spec.hs
else
   echo "No tests for $1"
fi

