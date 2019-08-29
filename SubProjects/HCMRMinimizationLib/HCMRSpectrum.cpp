#include "HCMRSpectrum.h"
#include <algorithm>
#include "loguru.h"

HCMRSpectrum::HCMRSpectrum() :data_Vector(), _name("") {}

HCMRSpectrum::HCMRSpectrum(std::vector<double> dataVector, std::string name) : data_Vector(dataVector), _name(name) {}

void HCMRSpectrum::addCountsInChannel(uint32_t channelNum, double counts)
{
	if (data_Vector.size() < channelNum + 1)
	{
		data_Vector.resize(channelNum + 1, 0);
	}
	data_Vector.at(channelNum) = counts;
}

void HCMRSpectrum::setName(std::string name)
{
	_name = name;
}

void HCMRSpectrum::setSpectrum(std::vector<double> dataVector)
{
	data_Vector = dataVector;
}

double HCMRSpectrum::getCountsInChannel(uint32_t channelNum) const
{
	if (data_Vector.size() < channelNum + 1)
	{
		LOG_F(WARNING, "Trying to access channel #%d in %s spectrum, that is out of range.", channelNum, _name.c_str());
		LOG_F(WARNING, "Returning zero counts");
		return 0.F;
	}
	else
	{
		return data_Vector.at(channelNum);
	}
}

std::string HCMRSpectrum::getName() const
{
	return _name;
}

const std::vector<double>& HCMRSpectrum::getDataVector() const
{
	return data_Vector;
}

uint32_t HCMRSpectrum::size() const
{
	return data_Vector.size();
}

void HCMRSpectrum::clearSpectrum()
{
	data_Vector.clear();
}

void HCMRSpectrum::print() const
{
	printf("Printing Spectrum with name: %s\n", _name.c_str());
	printf("----------------------------------------------\n");
	int count = 0;
	for (double value : data_Vector)
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
