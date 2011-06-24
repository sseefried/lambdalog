#!/bin/bash

TGT="mco:/home/sseefried/domains/lambdalog.seanseefried.com/public"
echo "Deploying to $TGT"
rsync -avz _site/* "$TGT"
