#!/bin/bash

TGT="playspace:/home/sseefried/sites/lambdalog.seanseefried.com/public"
# ghc-sandbox hakyll --make Site.hs -optl-Wl,-no_pie
cabal install
SITE=$(echo dist/dist-sandbox-*/build/Site/Site)
[ $? -eq 0 ] || (exit 1)
(cd static/sharing-recovery/graphviz; ./generate_pngs.sh)

$SITE clean
$SITE build
echo "Deploying to $TGT"
rsync -avz _site/* "$TGT"
