#ifndef HCMR_MINIMIZATION_H
#define HCMR_MINIMIZATION_H

#include "HCMRData.h"
#include "HCMRModel.h"
#include "Minuit2/FCNBase.h"

class HCMRFitManager : public ROOT::Minuit2::FCNBase
{
public:
	HCMRFitManager();
	void addData(const std::string& file);
	void addUnitaryToModel(const std::string& file);
	double computeChiSquare(const std::vector<double>& par) const;

	// FCNBase methods implementation
	double operator()(const std::vector<double>& par) const override;
	double Up() const override;
	HCMRData data_;
	HCMRModel _model;
};

#endif // HCMR_MINIMIZATION_H
