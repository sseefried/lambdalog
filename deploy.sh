#!/bin/bash


TGT="mco:/home/sseefried/domains/lambdalog.seanseefried.com/public"
ghc-hakyll --make Site.hs
[ $? -eq 0 ] || (exit 1)
./Site clean
./Site build
echo "Deploying to $TGT"
rsync -avz _site/* "$TGT"
