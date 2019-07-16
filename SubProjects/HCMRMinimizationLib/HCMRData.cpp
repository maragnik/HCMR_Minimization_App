#include "HCMRData.h"

HCMRData::HCMRData() : _spectrum() {}

const HCMRSpectrum& HCMRData::getSpectum() const
{
	return _spectrum;
}

void HCMRData::print() const
{
	std::cout << "\n\n########################################################" << std::endl;
	std::cout << "PRINTING DATA" << std::endl;
	std::cout << "########################################################\n" << std::endl;
	int count = 0;
	count++;
	std::cout << "Unitary #" << count << std::endl;
	_spectrum.print();
}

void HCMRData::fillData(const std::string& file)
{
	_spectrum.fillFromFile(file);
}
