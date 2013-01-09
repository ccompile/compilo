#!/bin/bash

./minic $1 > /tmp/test.s
spim -file /tmp/test.s | tail -n +6

