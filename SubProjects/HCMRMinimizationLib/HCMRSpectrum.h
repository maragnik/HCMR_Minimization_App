#ifndef HCMRSPECTRUM_H
# define HCMRSPECTRUM_H

#include "common_definitions.h"
#include <fstream>
#include <iostream>
#include <limits>
#include <vector>

class HCMRSpectrum
{
public:
	HCMRSpectrum();
	HCMRSpectrum(std::vector<double> dataVector, std::string name);

	void addCountsInChannel(uint32_t channelNum, double counts);
	void setName(std::string name);
	void setSpectrum(std::vector<double> dataVector);

	double getCountsInChannel(uint32_t channelNum) const;
	std::string getName() const;
	const std::vector<double>& getDataVector() const;
	uint32_t size() const;
	void clearSpectrum();
	void print() const;

private:
	std::vector<double> _dataVector;
	std::string _name;
};

#endif // HCMRSPECTRUM_H
