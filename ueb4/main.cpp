#include <mpi.h>
#include <iostream>

int main(int argc, char* argv[]) {

	MPI_Init(&argc, &argv);

    int numprocs;
    int processid;

	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &processid);

    std::cout << "Process ID is: " << processid << std::endl;

    int i;

    for(i = 1; i < argc; i++) {
        std::cout << argv[i] << std::endl;
    }

	MPI_Finalize();

	return 0;
}
