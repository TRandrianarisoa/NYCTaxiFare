#!/bin/sh

## This script extract a specified file from the zip archive
## of the competition.
## The file is sorted and stored compressed with the same name 
## plus a .gz extension
## Temporary files for sorting are created in the current directory
## unzip_sort.sh raw-data.zip file-to-extract.csv
## Example: unzip_sort.sh raw.zip train.csv 

unzip $1 $2 -c | tail -n +3 | sort -k3 -t, --compress-program=gzip -S 50% -T. -o $2.gz
