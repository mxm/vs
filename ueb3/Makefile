compile:
	gcc sum.c -o sum -fopenmp

clean:
	rm sum

run:
	OMP_NUM_THREADS=2 ./sum 4096
	OMP_NUM_THREADS=4 ./sum 4096
	OMP_NUM_THREADS=8 ./sum 1024
	OMP_NUM_THREADS=16 ./sum 1024
