﻿#ifndef HCMR_MINIMIZATION_H
#define HCMR_MINIMIZATION_H

#include "HCMRData.h"
#include "HCMRModel.h"
#include "Minuit2/FCNBase.h"

class HCMRMinimization : public ROOT::Minuit2::FCNBase
{
public:
	HCMRMinimization();
	void addData(const std::string& file);
	void addUnitaryToModel(const std::string& file);
	double computeChiSquare(const std::vector<double>& par) const;

	// FCNBase methods implementation
	double operator()(const std::vector<double>& par) const override;
	double Up() const override;
	HCMRData _data;
	HCMRModel _model;
};

#endif // HCMR_MINIMIZATION_H