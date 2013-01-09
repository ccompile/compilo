#!/bin/bash

good=0
bad=0

check=`ls . | grep minic`
if [ $? -eq 1 ]; then
    echo "Executable ./minic was not found."
else

for dir in `cat tests/targets/$1-0`; do
    for file in `ls tests/$dir | grep \\\\.c`; do
        ./minic $2 tests/$dir/$file > /tmp/test.s
        timeout  3 spim -file /tmp/test.s | tail -n +6 > /tmp/test.out 
        retcode=$?
        if [ $retcode -ne 0 ];  then
            echo -e "Test $dir/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : exit code is $retcode, expected 0.";
            ((bad++));
        else
            basename=`echo $file | sed s/\\\\.c//`
            diff /tmp/test.out tests/$dir/$basename.out
            retcode=$?
            if [ $retcode -ne 0 ]; then
                echo -e "Test $dir/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : wrong answer.";
                ((bad++));
            else
                echo -e "Test $dir/$file \e[0;32mpassed\e[00m.";
                ((good++));
            fi
        fi
    done;
done;

if [ $bad -ne 0 ]; then
    echo -e "Tests $1 with $2 : \e[0;32m$good passed\e[00m, \e[01;31m$bad failed\e[00m."
else
    echo -e "Tests $1 with $2 : \e[0;32m$good passed, $bad failed\e[00m."
fi

fi

