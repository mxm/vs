#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <omp.h>
#include <time.h>

int main(int argc, char *argv[]) {

    int i, n, sum;

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

#pragma omp parallel for reduction(+:sum)
    //compute the sum
    for(i = 0; i < n; i++) {
        sum = sum + input[i];
    }

    assert(sum == n);

    printf("Sum is: %d\n", sum);

    exit(0);

}
