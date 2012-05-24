std::map<std::string, int> mymap(std::string filename);
void printmap(std::map<std::string, int>);

std::map<std::string, int> mapdeserialize(std::string serializedmap);
std::string mapserialize(std::map<std::string, int> map);
