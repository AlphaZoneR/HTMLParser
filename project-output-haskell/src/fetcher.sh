#!/bin/bash

while true
do
	clear
	./parseJSONtoTree "$1" > App.js
	cat App.js
	sleep 5
done
