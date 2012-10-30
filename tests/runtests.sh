#!/bin/bash

good=0
bad=0

for dir in `cat tests/targets/$1-0`; do
    for file in `ls tests/$dir | grep \.c`; do
        ./minic $2 tests/$dir/$file > /tmp/minic_test_out
        retcode=$?
        if [ $retcode -ne 0 ];  then
            echo -e "Test $dir/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : exit code is $retcode, expected 0.";
            ((bad++));
        else
            echo -e "Test $dir/$file \e[0;32mpassed\e[00m.";
            ((good++));
        fi
    done;
done;

for dir in `cat tests/targets/$1-1`; do
    for file in `ls tests/$dir | grep \.c`; do
        ./minic $2 $file > /tmp/minic_test_out
        retcode=$?
        if [ $retcode -ne 1 ];  then
            echo -e "Test $dir/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : exit code is $retcode, expected 1.";
            ((bad++))
        else
            echo -e "Test $dir/$file \e[0;32mpassed\e[00m.";
            ((good++))
        fi
    done;
done;

if [ $bad -ne 0 ]; then
    echo -e "Tests $1 with $2 : \e[0;32m$good passed\e[00m, \e[01;31m$bad failed\e[00m."
else
    echo -e "Tests $1 with $2 : \e[0;32m$good passed, $bad failed\e[00m."
fi

