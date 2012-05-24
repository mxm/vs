#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <omp.h>

/* divisor used for splitting up threads */
int divisor = 2;

long long fib(int n, int num_threads, int thread_id) {
   	long long fib1, fib2;

	int num_threads_left;
	int r;

	if (n < 2) {
		return n;
	}
	
	/* Now we sort sequentially because we ran out of threads */
	if (num_threads <= 1) {
		printf("No threads left, now computing sequentially => [ID: %d][n: %d]\n", thread_id, n);
		return (fib(n-1, 1, thread_id) + fib(n-2, 1, thread_id));
	}
	
	/* Divide the number of threads into two parts according to divisor */
	num_threads_left = num_threads / divisor + num_threads % divisor;
	//printf("threads: %ld => %ld, %ld\n", num_threads, num_threads_left, num_threads-num_threads_left);

	/* Compute in parallel */
	#pragma omp parallel sections num_threads(num_threads)
	{
	  /* Thread ID counter increased atomicly before start of sections */
	  #pragma omp atomic
		  thread_id++;
	  
	  #pragma omp section
		  fib1 = fib(n-1, num_threads_left, thread_id);
		  
	  #pragma omp section
		  fib2 = fib(n-2, num_threads-num_threads_left, thread_id+1);
	}
	return(fib1+fib2);
}


int main(int argc, char *argv[]) {

    int n, num_threads;
    long long int result;

	/* Get input parameters */
    assert(argc > 2);
	n = atoi(argv[1]);
	num_threads = atoi(argv[2]);
	assert(num_threads >= 0);
    assert(n >= 0);

	/* Set divisor if parameter is given */
	if(argc > 3)
		divisor = atoi(argv[3]);
	printf("divisor: %d\n", divisor);
   
	/* Computer fib(n) */ 
    result = fib(n, num_threads, 0);
    
    printf("fib(%d) = %lld\n", n, result);

    return 0;

}
