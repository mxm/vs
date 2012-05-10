#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

void replaceAll(std::string& str, const std::string& from, const std::string& to) {
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

void map(std::string filename) {
#ifdef DEBUG
    std::cout << "Filename is: " << filename << std::endl;
#endif

    std::string word;
    std::string line;

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
#ifdef DEBUG
                std::cout << word << std::endl;
#endif
            }
        }
    }
}


//Vector<Map<String, Int>> map(String filename) {
//
//
//}

int main() {
    map("sample.txt");
    return 0;
}
