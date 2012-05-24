compile:
	mpicxx main.cpp map.cpp -o main

clean:
	rm -r data
	mkdir data
	rm main

test:
	mpicxx map.cpp -o map -DDEBUG
	mpirun map
	mpicxx main.cpp map.cpp -o main
	mpirun -n 4 main sample.txt sample.txt sample2.txt

run:
	mpirun -n 2 main sample.txt sample.txt sample.txt
	ls -lh data
	cat data/*

run8:
	mpirun -n 2 main sample.txt sample.txt sample.txt sample.txt sample.txt sample.txt sample.txt sample.txt
	ls -lh data
	cat data/*
