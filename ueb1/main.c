#include <mpi.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

/* First argument should be the number of elements in array */
int main (int argc, char *argv[]){
	int numprocs;
	int workers;
	int myid;

	int* array;
	int* buf;
	int i = 0;
	int res = 0;
	MPI_Request req1;
	MPI_Request req2;

	/* Initialize MPI */
	MPI_Init(&argc,&argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &myid);

	/*Quit if we only have one process */
	assert(numprocs > 1);
	workers = numprocs - 1;

	/* Convert input to number */
	int n = atoi(argv[1]);
	assert(n > 0);

	/* Size of a normal chunk and size of remainder */
	int chunksize = n/workers;
	int rem = n%workers;


	/* Process 0 creates array, and sends it to the remaining processes */
	if(myid == 0){
		/* Create Array with 1s */
		printf("Creating Array with size %i\nWill send this array to %i processes\n", n, workers);
		array = malloc(n * sizeof(int*));
		for (i = 0; i < n; i++)
			array[i] = 1;

		int offset = 0;
		/* Send to other processes*/
		for(i = 1; i <= workers; i++){
			int count = chunksize + (rem-- > 0 ? 1 : 0);
			/* Send number of elements */
			MPI_Isend(&count,1, MPI_INT, i, 1, MPI_COMM_WORLD, &req1);
			/* Send actual elements */
			MPI_Isend(array+offset, count, MPI_INT, i, 2, MPI_COMM_WORLD, &req2);
			printf("Sending %i elements to worker %i\n", count, i);
			offset += count;
		}

		printf("Sending done. Now receiving...\n");

		/* Receive sums from workers */
		int sum = 0;
		int totalsum = 0;
		for(i = 1; i <= workers; i++){
			res = MPI_Recv(&sum, 1, MPI_INT, i, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			assert(res == MPI_SUCCESS);
			printf("Sum received => sum: %i\n",sum);
			totalsum += sum;
		}

		printf("The total sum is: %i\n", totalsum);
		assert(totalsum == n);

	/* Other processes sum up received array elements */
	}else{
		/* Receive number of elements */
		int count = 0;
		res = MPI_Recv(&count, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		assert(res == MPI_SUCCESS);

		buf = malloc(count*sizeof(int*));

		/* Receive actual elements */
		res = MPI_Recv(buf, count, MPI_INT, 0, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		assert(res == MPI_SUCCESS);

		int sum = 0;
		for(i = 0; i < count; i++)
			sum += buf[i];

		//printf("Sum calculated by process %i => sum: %i\n",myid,sum);

		/* Send sum to master */
		res = MPI_Send(&sum, 1, MPI_INT, 0, 1, MPI_COMM_WORLD);
		assert(res == MPI_SUCCESS);
	}

	MPI_Finalize();
	return 0;
}
