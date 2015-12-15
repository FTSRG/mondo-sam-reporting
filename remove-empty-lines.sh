#!/bin/bash

for f in "*.csv"
do
  echo $f
  # remove
  # 1) empty lines
  # 2) lines only containing a comma
  sed -i '/^,*$/d' $f
done
