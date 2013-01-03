#!/bin/sh

for file in `find . -name \*.html -print`; do
    rm $file
done

