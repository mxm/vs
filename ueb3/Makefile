compile:
	gcc sum.c -o sum -fopenmp
	gcc fib.c -o fib -fopenmp

clean:
	rm sum fib

run:
	OMP_NUM_THREADS=2 ./sum 4096
	OMP_NUM_THREADS=4 ./sum 4096
	OMP_NUM_THREADS=8 ./sum 1024
	OMP_NUM_THREADS=16 ./sum 1024

fibrun:
	./fib 0
	./fib 1
	./fib 2
	./fib 3
	./fib 42
