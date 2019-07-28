#ifndef HCMR_PARSER_H
#define HCMR_PARSER_H

#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream> 
#include "HCMRData.h"

class HCMRParser
{
public:
	HCMRParser();
	~HCMRParser();

	bool parseSpeDataFile(const std::string& file, HCMRData* data);

	template<class T>
	void parseLine(const std::string& line, std::vector<T>& outVector);
	std::vector<std::string> splitPath(const std::string& file);


private:
	std::ifstream _stream;

	HCMRParser(const HCMRParser&) = delete;
	HCMRParser& operator=(const HCMRParser&) = delete;
};

#endif // HCMR_PARSER_H
