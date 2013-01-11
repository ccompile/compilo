#!/bin/sh

for file in `find . -name \*.html -print`; do
    rm $file
done

for file in `find . -name \*.s -print`; do
    rm $file
done

