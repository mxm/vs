#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <omp.h>


long long int fib(int n) {
    if(n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fib(n-1) + fib(n-2);
    }
}

int main(int argc, char *argv[]) {

    int n;

	/* Convert input to number */

    assert(argv[1] != NULL);

	n = atoi(argv[1]);

    assert(n >= 0);

    printf("fib(%d) = %lld\n", n, fib(n));

    exit(0);

}
