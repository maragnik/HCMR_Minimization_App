#ifndef HCMR_MODEL_H
#define HCMR_MODEL_H

#include "HCMRSpectrum.h"
#include <vector>

class HCMRModel
{
public:
	HCMRModel();
	void addNewUnitary(const std::string& file);
	void print() const;
	typeY getY(const std::vector<double>& par, typeX x) const;
private:
	std::vector<HCMRSpectrum> _unitaries;
};

#endif // HCMR_MODEL_H
