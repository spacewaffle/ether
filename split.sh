#!/bin/bash

while read x 
do

  chan=$(echo -n "$x" | jq -M -c -r  .chan )
  echo "$x" >> $chan.json
done

