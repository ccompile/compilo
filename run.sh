#!/bin/bash

./minic $1 > /tmp/test.s
spim -file /tmp/test.s $2 $3 $4 $5 $6 | tail -n +6

