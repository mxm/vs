compile:
	mpicxx main.cpp map.cpp -o main

clean:
	rm map main

test:
	mpicxx map.cpp -o map -DDEBUG
	mpirun map

run:
	mpirun -n 2 main sample.txt sample.txt sample.txt
