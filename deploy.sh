#!/bin/bash

TGT="playspace:/home/sseefried/sites/lambdalog.seanseefried.com/public"
echo "Deploying to $TGT"
rsync -avz _site/* "$TGT"
