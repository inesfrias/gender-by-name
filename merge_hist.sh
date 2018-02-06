#!/bin/bash

cat /dev/null > ./output/pers_hist_features.csv

for i in `ls -rt $1*.csv`; do
	echo $i
	sed 1d $i >> ./output/pers_hist_features.csv
done
