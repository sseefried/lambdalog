#!/bin/bash


for i in *.dot; do 

dot $i -Tpng -o `basename $i .dot`.png
mv *.png ../images

done

