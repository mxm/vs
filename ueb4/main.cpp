#include <mpi.h>
#include <iostream>
#include <vector>
#include <string>

#include "map.hpp"

int main(int argc, char* argv[]) {

	MPI_Init(&argc, &argv);

    int numprocs;
    int processid;

	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &processid);

#ifdef DEBUG
    std::cout << "Process ID is: " << processid << std::endl;
#endif

    std::vector<std::string> files;

    int i = 1 + processid;

    //each process should pick his files
    //we don't need a fancy distribution function so far
    for(i ; i < argc; i = i + numprocs) {
        files.push_back(std::string(argv[i]));
    }

    std::vector<std::string>::iterator end = files.end();

    for(std::vector<std::string>::iterator iter = files.begin(); iter != end; ++iter) {
        printmap(map(*iter));
    }

	MPI_Finalize();

	return 0;
}
