#!/bin/bash

m=$(($1 % 2 + 1))
x=$(($1 / 2))
k=$(($x / 2 + 1))

echo $k
echo $m
Rscript run.R $k $m
