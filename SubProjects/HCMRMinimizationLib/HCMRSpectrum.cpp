#include "HCMRSpectrum.h"
#include <algorithm>
#include "loguru.h"

HCMRSpectrum::HCMRSpectrum() :_dataVector(), _name("") {}

HCMRSpectrum::HCMRSpectrum(std::vector<double> dataVector, std::string name) : _dataVector(dataVector), _name(name) {}

void HCMRSpectrum::addCountsInChannel(uint32_t channelNum, double counts)
{
	if (_dataVector.size() < channelNum + 1)
	{
		_dataVector.resize(channelNum + 1, 0);
	}
	_dataVector.at(channelNum) = counts;
}

void HCMRSpectrum::setName(std::string name)
{
	_name = name;
}

void HCMRSpectrum::setSpectrum(std::vector<double> dataVector)
{
	_dataVector = dataVector;
}

double HCMRSpectrum::getCountsInChannel(uint32_t channelNum) const
{
	if (_dataVector.size() < channelNum + 1)
	{
		LOG_F(WARNING, "Trying to access channel #%d in %s spectrum, that is out of range.", channelNum, _name.c_str());
		LOG_F(WARNING, "Returning zero counts");
		return 0.F;
	}
	else
	{
		return _dataVector.at(channelNum);
	}
}

std::string HCMRSpectrum::getName() const
{
	return _name;
}

const std::vector<double>& HCMRSpectrum::getDataVector() const
{
	return _dataVector;
}

uint32_t HCMRSpectrum::size() const
{
	return _dataVector.size();
}

void HCMRSpectrum::clearSpectrum()
{
	_dataVector.clear();
}

void HCMRSpectrum::print() const
{
	printf("Printing Spectrum with name: %s\n", _name.c_str());
	printf("----------------------------------------------\n");
	int count = 0;
	for (double value : _dataVector)
	{
		if (count > 8)
		{
			count = 0;
			std::cout << value << std::endl;
		}
		else
		{
			std::cout << value << " ";
		}
		count++;
	}
	printf("\n===============================================\n");
}
