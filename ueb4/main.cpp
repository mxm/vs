#include <mpi.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <stdlib.h> //malloc

#include "map.hpp"

//stolen from http://www.cse.yorku.ca/~oz/hash.html
//generate a hash (unsigned long) from a string
//used to assign words to processes
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

    //get number of process and process id
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &processid);

    std::vector<std::string> files;

    int i = 1 + processid;

    //each process should pick his files
    //we don't need a fancy distribution function so far
    for(i ; i < argc; i = i + numprocs) {
        files.push_back(std::string(argv[i]));
    }

    //container for results from the local map step
    std::map<std::string, int> localresult;

    //iterate over all files
    //collect results in vector
    std::vector<std::string>::iterator end = files.end();

    //map step
    //do map and merge local map results
    for(std::vector<std::string>::iterator iter = files.begin(); iter != end; ++iter) {
        mergemaps(localresult, map(*iter));
    }

    //initialize array for data that belongs to remote processes
    std::vector<std::map<std::string, int> > remoteresults;
    for(i = 0; i < numprocs; ++i) {
        remoteresults.push_back(std::map<std::string, int>());
    }

    //for each word in the local result set
    //hash the word with the hash function
    //assign word to process
    //use modulo process number
    //should garantee a even distribution over all processes
    std::map<std::string, int>::iterator end2 = localresult.end();
    for(std::map<std::string, int>::iterator iter = localresult.begin(); iter != end2; ++iter) {
        int processid = (int) hash(iter->first) % numprocs;
        remoteresults[processid][iter->first] = iter->second;
    }

    //send local results that don't belong to process to n-1 other processes

    std::map<std::string, int> reduceresults;

    for(i = 0; i < numprocs; ++i) {
        if(i == processid) {
            //do not send data to yourself
            //merge with reduce results ;)
            mergemaps(reduceresults, remoteresults[i]);
        } else {
            //MPI_send to n-1 process
            //send a string that represents the map for process x
            std::string payload = mapserialize(remoteresults[i]);
            MPI_Send((void *)(payload.c_str()), strlen(payload.c_str())+1, MPI_CHAR, i, 1, MPI_COMM_WORLD);
        }
    }

    //recv from n-1 process
    for(i = 0; i < numprocs; ++i) {
        if(i != processid) {

            //TODO this is insane shit... simplify this!
            int count;
            MPI_Status status;
            MPI_Probe(i, 1, MPI_COMM_WORLD, &status);
            MPI_Get_count(&status, MPI_CHAR, &count);
            char* buffer = (char*) malloc(sizeof(char) * count); //is this c++ style?

            MPI_Recv(buffer, count, MPI_CHAR, i, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

            //do not merge empty data
            if(std::string(buffer) != "") {
                //reduce data from n-1 processes into one map
                mergemaps(reduceresults, mapdeserialize(std::string(buffer)));
            }
        }
    }

    //write local data to file
    //end of reduce step
    std::stringstream ss;
    ss << "./data/reduced-" << processid;

    std::ofstream reducefile(ss.str().c_str());

    std::map<std::string, int>::iterator end4 = reduceresults.end();
    for(std::map<std::string, int>::iterator iter = reduceresults.begin(); iter != end4; ++iter) {
        reducefile << iter->first << ":" << iter->second << std::endl;
    }

    reducefile.close();

    MPI_Finalize();

    return 0;
}
