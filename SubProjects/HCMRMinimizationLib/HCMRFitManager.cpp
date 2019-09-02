#include "HCMRFitManager.h"

HCMRFitManager::HCMRFitManager() {}

void HCMRFitManager::addData(const std::string& file)
{
	data_.fillDataFromSpeFile(file);
}

void HCMRFitManager::addUnitaryToModel(const std::string& file)
{

}

double HCMRFitManager::computeChiSquare(const std::vector<double>& par) const
{
	double chi2 = 0;
	return chi2;
}

inline double HCMRFitManager::operator()(const std::vector<double>& par) const
{
	return computeChiSquare(par);
}

inline double HCMRFitManager::Up() const
{
	return 1.;
}
