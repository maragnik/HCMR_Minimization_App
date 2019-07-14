#ifndef HCMR_DATA_H
#define HCMR_DATA_H

#include "HCMRSpectrum.h"

class HCMRData : public HCMRSpectrum
{
public:
	HCMRData();
	const HCMRSpectrum& getSpectum() const;
private:
	HCMRSpectrum _data;
};

#endif // HCMR_DATA_H
