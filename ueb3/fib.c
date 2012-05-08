#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <omp.h>


long long int fib(int n, int num_threads, int thread_id) {
   	 long fib1, fib2;

	int num_threads_left;
	
	if (n < 2) {
		return n;
	}
	
	#pragma omp
	if (num_threads <=1) {
		printf("[ID: %d][n: %d]\n", thread_id, n);
		return (fib(n-1, 1, thread_id)+ fib(n-2, 1, thread_id));
	}
	
	if ((num_threads % 2) == 1)
	    num_threads_left = num_threads / 2 + 1;
        else
	    num_threads_left = num_threads / 2;

	#pragma omp parallel sections num_threads(2)
	{
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

	/* Convert input to number */

    assert(argv[1] != NULL);

	n = atoi(argv[1]);
	num_threads = atoi(argv[2]);

    assert(n >= 0);
    
    result = fib(n, num_threads, 0);
    
    printf("fib(%d) = %lld\n", n, result);

    exit(0);

}
