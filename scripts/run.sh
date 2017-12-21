#!/bin/bash

# Run input file through the executable
cat input/AOC$1.txt | cabal exec runhaskell -- -isrc src/AOC$1
