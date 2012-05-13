#include <mpi.h>
#include <iostream>
#include <vector>
#include <string>
#include <map>

#include "map.hpp"

//stolen from http://www.cse.yorku.ca/~oz/hash.html
unsigned long hash(std::string s) {
    unsigned long hash = 5381;

    for(std::string::const_iterator iter = s.begin(); iter != s.end(); ++iter) {
        hash = ((hash << 5) + hash) + *iter; /* hash * 33 + c */
    }
    return hash;
}

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

    std::vector<std::map<std::string, int> > remoteresults;

    for(i = 0; i < numprocs; ++i) {
        remoteresults.push_back(std::map<std::string, int>());
    }

    std::map<std::string, int>::iterator end2 = localresult.end();
    for(std::map<std::string, int>::iterator iter = localresult.begin(); iter != end2; ++iter) {
#ifdef DEBUG
        std::cout << hash(iter->first) % numprocs << std::endl;
#endif
        int processid = (int) hash(iter->first) % numprocs;
        remoteresults[processid][iter->first] = iter->second;
    }

    //send local results that don't belong to process to n-1 other processes
    //what happens if number of files is smaller then process number?
    //do not communicate with all process?

    for(i=0; i < numprocs; ++i) {
#ifdef DEBUG
        std::cout << mapserialize(remoteresults[i]) << std::endl;
#endif
        if(i == processid) {
            //do not send data to yourself
            //merge with reduce results ;)
        } else {
            //MPI_send to n-1 process
            //send a string that represents the map for process x
        }
    }

    //receive n-1 serialized maps from n-1 other process
    //empty string equals no data?

	MPI_Finalize();

	return 0;
}
