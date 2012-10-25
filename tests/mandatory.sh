#!/bin/bash

for file in `ls tests/$1 | grep .c`; do
    ./minic $2 $file;
    if [ $? -ne $3 ]; then
        echo -e "\e[01;31mTest\e[00m $1/\e[0;33m$file\e[00m \e[01;31mfailed\e[00m : exit code is $?, expected $3.\nOptions were : $2.";
        exit 1
    fi
done


