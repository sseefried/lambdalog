#!/bin/bash


for i in *.dot; do 

NAME=`basename $i .dot`.png
dot $i -Tpng -o $NAME
echo "Generating $NAME"

done

mv *.png ../images
