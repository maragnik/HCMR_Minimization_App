#include "HCMRParser.h"
#include "loguru.h"
#include <algorithm>

HCMRParser::HCMRParser()
{}

HCMRParser::~HCMRParser()
{
	_stream.close();
}

template<class T>
void HCMRParser::parseLine(const std::string& line, std::vector<T>& outVector)
{
	outVector.clear();
	std::stringstream ss;

	/* Storing the whole string into string stream */
	ss << line;

	/* Running loop till the end of the stream */
	std::string temp;
	T found;
	while (!ss.eof()) {

		/* extracting word by word from stream */
		ss >> temp;

		/* Checking the given word is integer or not */
		if (std::stringstream(temp) >> found)
		{
			outVector.push_back(found);
		}

		/* To save from space at the end of string */
		temp = "";
	}
}

bool HCMRParser::parseSpeDataFile(const std::string& file, HCMRData* data)
{
	std::vector<std::string> sourceFileInfo = splitPath(file);
	std::string filename = (sourceFileInfo.size() == 3) ? sourceFileInfo[0] + sourceFileInfo[1] : sourceFileInfo[0];

	LOG_F(INFO, "Parsing file %s", file.c_str());
	_stream.open(file);

	if (_stream.fail())
	{
		LOG_F(ERROR, "Could not open file %s!", filename.c_str());
		_stream.close();
		return false;
	}


	std::vector<uint32_t> intVector;
	std::vector<double> doubleVector;
	std::vector<std::string> stringVector;

	std::string line = "";
	int lineNum = 0;
	std::string searchString[] = { "DATA COLLECTION STARTED ON:",
								   "STARTING TIME:",
								   "ELAPSED LIVE TIME (sec):",
								   "ELAPSED REAL TIME (sec):" };
	bool endFound = false;
	uint32_t energyChannel = 0;
	while (std::getline(_stream, line))
	{
		if (line.size() > 0)
		{
			++lineNum;
			if (lineNum > 2 && !endFound)
			{
				if (line.find(searchString[0]) == std::string::npos)
				{
					parseLine<double>(line, doubleVector);
					if (doubleVector.size() != 10)
					{
						LOG_F(WARNING, "Line number %d of file %s has only %d values!", lineNum, filename.c_str(), static_cast<int>(doubleVector.size()));
					}
					for (double value : doubleVector)
					{
						data->_spectrum.addCountsInChannel(energyChannel, value);
						energyChannel++;
					}
				}
				else
				{
					parseLine<std::string>(line.substr(searchString[0].size()), stringVector);
					if (!stringVector.empty())
					{
						data->_dateCollected = stringVector[0];
					}
					else
					{
						LOG_F(WARNING, "File %s has no %s info!", filename, searchString[0]);
					}
					endFound = true;
				}
			}
			else if (lineNum == 1)
			{
				parseLine<uint32_t>(line, intVector);
				if (intVector.size() > 0)
				{
					data->_numOfChanels = intVector[0];
				}
				else
				{
					LOG_F(ERROR, "File %s does not have info about number of energy channels!", filename.c_str());
					return false;
				}
			}
			else if (lineNum == 2)
			{
				parseLine<double>(line, doubleVector);
				if (doubleVector.size() >= 2)
				{
					data->_offsetInKeV = doubleVector[0];
					data->_keVperChannel = doubleVector[1];
					data->_isCalibrated = (static_cast<int>(data->_offsetInKeV) != 0) ||
						(data->_keVperChannel > 1.001);
				}
				else
				{
					LOG_F(ERROR, "File %s does not have calibration info!", filename.c_str());
					return false;
				}
			}
			else if (endFound)
			{
				if (line.find(searchString[1]) != std::string::npos)
				{
					parseLine<std::string>(line.substr(searchString[1].size()), stringVector);
					if (!stringVector.empty())
					{
						data->_timeCollected = stringVector[0];
					}
					else
					{
						LOG_F(WARNING, "File %s has no %s info!", filename.c_str(), searchString[1].c_str());
					}
				}
				else if (line.find(searchString[2]) != std::string::npos)
				{
					parseLine<uint32_t>(line.substr(searchString[2].size()), intVector);
					if (!intVector.empty())
					{
						data->_liveMeassurementDuration = intVector[0];
					}
					else
					{
						LOG_F(WARNING, "File %s has no %s info!", filename.c_str(), searchString[2].c_str());
					}
				}
				else if (line.find(searchString[3]) != std::string::npos)
				{
					parseLine<uint32_t>(line.substr(searchString[3].size()), intVector);
					if (!intVector.empty())
					{
						data->_realMeassurementDuration = intVector[0];
					}
					else
					{
						LOG_F(WARNING, "File %s has no %s info!", filename.c_str(), searchString[3].c_str());
					}
				}
				else
				{
					LOG_F(WARNING, "File %s has no missing info!", filename.c_str());
				}
			}
		}
	}
}

std::vector<std::string> HCMRParser::splitPath(const std::string & file)
{
	std::vector<std::string> name;
	size_t pos = std::min(file.find_last_of("\\"), file.find_last_of("/"));
	size_t posExt = file.find_last_of(".");
	if (file.empty() || (pos == std::string::npos))
	{
		name.push_back("Undefined");
		return name;
	}

	if (pos < posExt)
	{
		name.push_back(file.substr(pos + 1, posExt - pos - 1));
		name.push_back(file.substr(posExt));
		name.push_back(file.substr(0, pos));
	}
	else
	{
		name.push_back(file.substr(pos + 1));
	}
	return name;
}
