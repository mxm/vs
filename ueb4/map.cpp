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
            while (getline(linestream, word, ' ')) {
                ++wordcounts[word];
            }
        }
    } else {
        //TODO some error handling?
    }

    return wordcounts;
}

int main() {
    std::map<std::string, int> result = map("sample.txt");
    std::map<std::string, int>::iterator end = result.end();
    for(std::map<std::string, int>::iterator iter = result.begin(); iter != end; ++iter) {
        std::cout << iter->first << ": " << iter->second << std::endl;
    }
    return 0;
}
