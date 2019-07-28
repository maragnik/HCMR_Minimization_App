#ifndef HCMR_MODEL_H
#define HCMR_MODEL_H

#include "HCMRSpectrum.h"
#include <vector>

class HCMRModel
{
public:
	HCMRModel();
private:
	std::vector<HCMRSpectrum> _unitaries;
};

#endif // HCMR_MODEL_H
