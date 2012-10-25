#!/bin/bash

good=0
bad=0

for file in `ls tests/$1 | grep .c`; do
    ./minic $2 $file > /tmp/minic_test_out
    retcode=$?
    if [ $retcode -ne $3 ]; then
        echo -e "Test $1/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : exit code is $retcode, expected $3.";
        ((bad++))
    else
        echo -e "Test $1/$file \e[0;32mpassed\e[00m."
        ((good++))
    fi
done;

if [ $bad -ne 0 ]; then
    echo -e "Tests $1 with $2 : \e[0;32m$good passed\e[00m, \e[01;31m$bad failed\e[00m."
else
    echo -e "Tests $1 with $2 : \e[0;32m$good passed, $bad failed\e[00m."
fi
