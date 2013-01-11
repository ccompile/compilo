#!/bin/bash

./minic $1
fname=`echo $1 | sed s/\\\\.c//`.s
spim -file $fname $2 $3 $4 $5 $6 | tail -n +6

