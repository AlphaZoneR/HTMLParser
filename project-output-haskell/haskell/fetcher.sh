#!/bin/bash

while true
do
	clear
	./parseJSONtoTree "$1" > ../src/App.js
	cat ../src/App.js
	sleep 5
done
