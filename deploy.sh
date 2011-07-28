#!/bin/bash

TGT="mco:/home/sseefried/domains/lambdalog.seanseefried.com/public"
ghc-sandbox hakyll --make Site.hs -optl-Wl,-no_pie
[ $? -eq 0 ] || (exit 1)
(cd static/sharing-recovery/graphviz; ./generate_pngs.sh)
./Site clean
./Site build
echo "Deploying to $TGT"
rsync -avz _site/* "$TGT"
