#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <omp.h>
#include <sys/time.h>

int main(int argc, char *argv[]) {

    int i, n, sum;
    struct timeval start, stop;

    sum = 0;

	/* Convert input to number */

    assert(argv[1] != NULL);

	n = atoi(argv[1]);

    assert(n > 0);

    int input[n];


#pragma omp parallel
    //print number of threads
    if (omp_get_thread_num() == 0) {
        printf("Using %d threads in parallel sections.\n", omp_get_num_threads());
    }


    // initialize array with 1's
#pragma omp parallel for
    for(i = 0; i < n; i++) {
        input[i] = 1;
    }

    gettimeofday(&start, NULL);

#pragma omp parallel for reduction(+:sum)
    //compute the sum
    for(i = 0; i < n; i++) {
        sum = sum + input[i];
    }

    gettimeofday(&stop, NULL);

    assert(sum == n);

    //printf("Sum is: %d\n", sum);
    int micro_secs = stop.tv_usec - start.tv_usec;
    printf("Time: %d microsec, %d millisec, %d seconds\n", micro_secs, micro_secs/1000, micro_secs/1000/1000);

    exit(0);

}
