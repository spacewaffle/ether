#!/bin/bash


for x in $(seq 1 70) ; do
  echo $x
  curl http://localhost:8081/sse?chan=a &
  sleep 0.01
done

for x in $(seq 1 100) ; do
  echo $x
  curl -d '{"body":"one","chan":"a","name":"dan0","type":"chat_message"}' http://localhost:8081/message
  sleep 0.05
done
