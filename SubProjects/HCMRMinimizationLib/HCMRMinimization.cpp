#include "HCMRMinimization.h"

HCMRMinimization::HCMRMinimization() {}

void HCMRMinimization::addData(const std::string& file)
{
	data_.fillDataFromSpeFile(file);
}

void HCMRMinimization::addUnitaryToModel(const std::string& file)
{

}

double HCMRMinimization::computeChiSquare(const std::vector<double>& par) const
{
	double chi2 = 0;
	return chi2;
}

inline double HCMRMinimization::operator()(const std::vector<double>& par) const
{
	return computeChiSquare(par);
}

inline double HCMRMinimization::Up() const
{
	return 1.;
}
