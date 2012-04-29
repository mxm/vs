#!/bin/bash
mpicc main.c -o main
for k in 1 10 32 128 666 10000
do
	echo "++++++++++++++++++++++++";
	#echo "Now using array size $k";
	for i in {1..8}
	do
		echo "-------------------------------------";
		echo "Array size is $k. Using $i processes";
		echo "-------------------------------------";
	    mpirun -n $i main $k
	done
	echo "++++++++++++++++++++++++";
	echo "";
done
