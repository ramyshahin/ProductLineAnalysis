#!/bin/bash

fileList() {
  local files=files.txt
  cat $files
}

fileList|while read i; do
  stack run -- --filename $i --csv stats.csv 
done

