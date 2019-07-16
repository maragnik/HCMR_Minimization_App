#ifndef HCMR_DATA_H
#define HCMR_DATA_H

#include "HCMRSpectrum.h"

class HCMRData
{
public:
	HCMRData();
	const HCMRSpectrum& getSpectum() const;
	void print() const;
	void fillData(const std::string& file);
private:
	HCMRSpectrum _spectrum;
};

#endif // HCMR_DATA_H
