compile:
	mpicxx map.cpp -o map

clean:
	rm map

test:
	mpicxx map.cpp -o map -DDEBUG
	mpirun map
