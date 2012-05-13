#include <mpi.h>
#include <iostream>
#include <vector>
#include <string>
#include <map>

#include "map.hpp"


//merge a map into another map
void mergemaps(std::map<std::string, int>& result, std::map<std::string, int> map) {

    std::map<std::string, int>::iterator end = map.end();
    for(std::map<std::string, int>::iterator iter = map.begin(); iter != end; ++iter) {
        result[iter->first] = result[iter->first] + iter->second;
    }
}

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

    std::map<std::string, int> localresult;

    //std::vector<std::map<std::string, int> > mapresults;
    //                                    ^ i love this syntax
    //iterate over all files
    //collect results in vector
    std::vector<std::string>::iterator end = files.end();

    //do map and merge local map results
    for(std::vector<std::string>::iterator iter = files.begin(); iter != end; ++iter) {
        mergemaps(localresult, map(*iter));
    }

#ifdef DEBUG
    printmap(localresult);
#endif

    //send local results that don't belong to process to n-1 other processes
    //what happens if number of files is smaller then process number?
    //do not communicate with all process?


	MPI_Finalize();

	return 0;
}
