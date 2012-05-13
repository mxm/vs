#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <map>

void replaceAll(std::string& str, const std::string& from, const std::string& to) {
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

std::map<std::string, int> map(std::string filename) {

    std::string word;
    std::string line;

    std::map<std::string, int> wordcounts;

    std::ifstream input(filename.c_str());

    if(input.is_open()) {

        while(input.good()) {

            //read a line from input file
            getline(input, line);

            //handle special characters as whitespace
            replaceAll(line, ".", " ");
            replaceAll(line, ",", " ");
            replaceAll(line, ";", " ");
            replaceAll(line, ";", " ");
            replaceAll(line, ":", " ");
            replaceAll(line, "-", " ");
            replaceAll(line, "+", " ");
            replaceAll(line, "/", " ");
            replaceAll(line, "?", " ");
            replaceAll(line, "!", " ");
            replaceAll(line, "(", " ");
            replaceAll(line, ")", " ");
            replaceAll(line, "[", " ");
            replaceAll(line, "]", " ");

            //be case insensetive
            std::transform(line.begin(), line.end(), line.begin(), ::tolower);

            std::stringstream linestream(line);

            //get words, split by whitespaces
            while(getline(linestream, word, ' ')) {
                ++wordcounts[word];
            }
        }
    } else {
        //TODO some error handling?
    }

    return wordcounts;
}

std::string mapserialize(std::map<std::string, int> map) {

    std::stringstream result;

    std::map<std::string, int>::iterator end = map.end();
    for(std::map<std::string, int>::iterator iter = map.begin(); iter != end; ++iter) {
        result << iter->first << ":" << iter->second << " ";
    }

    return result.str();
}

//function to deserialize a map (string)
std::map<std::string, int> mapdeserialize(std::string serializedmap) {

    std::string tuple;
    std::map<std::string, int> map;
    std::stringstream input(serializedmap);

    std::string word;
    int count;
    size_t position = 0;

    //tokenize by whitespace
    while(getline(input, tuple, ' ')) {

        //find key-value delimiter
        //reconstruct word and wordcount
        position = tuple.find(":");
        word = tuple.substr(0,position);
        count = atoi(tuple.substr(position+1, tuple.length()).c_str());
        map[word] = count; //feed data into result map
    }

    return map;
}

//debug function to print a map
void printmap(std::map<std::string, int> map) {
    std::map<std::string, int>::iterator end = map.end();
    for(std::map<std::string, int>::iterator iter = map.begin(); iter != end; ++iter) {
        std::cout << iter->first << ":" << iter->second << std::endl;
    }
}

int main(int argc, char* argv[]) {
    //test of map function
    printmap(map("sample.txt"));
    std::cout << "########################" << std::endl;
    std::cout << mapserialize(map("sample.txt")) << std::endl;
    std::cout << "########################" << std::endl;
    printmap(mapdeserialize(mapserialize(map("sample.txt"))));

    return 0;
}
