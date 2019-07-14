#ifndef HCMRUNITARYINPUT_H
# define HCMRUNITARYINPUT_H

#include <map>
#include <string>
#include <fstream>
#include <iostream>

template<class X, class Y>
class HCMRUnitaryInput
{
public:
	HCMRUnitaryInput()
	{

	}
	void addPoint(X x, Y y)
	{
		_unitarySpectrum.insert(std::pair<X, Y>(x, y));
	}
	bool fillFromFile(std::string file)
	{
		std::ifstream inFile(file);

		if (!inFile)
		{
			std::cerr << "Could not open file " << file << std::endl;
			return false;
		}

		// read until you reach the end of the file
		for (std::string line; std::getline(inFile, line); )
		{
			std::cout << line << std::endl;
		}
	}
private:
	std::map<X, Y> _unitarySpectrum;
};

#endif // HCMRUNITARYINPUT_H
