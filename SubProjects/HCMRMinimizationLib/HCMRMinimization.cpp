#include "HCMRMinimization.h"

HCMRMinimization::HCMRMinimization() {}

void HCMRMinimization::addData(const std::string& file)
{
	_data.fillData(file);
}

void HCMRMinimization::addUnitaryToModel(const std::string& file)
{
	_model.addNewUnitary(file);
}

double HCMRMinimization::computeChiSquare(const std::vector<double>& par) const
{
	double chi2 = 0;
	for (const PointType& point : _data.getSpectum().getXYData())
	{
		YType modelY = _model.getY(par, point.first);
		YType diff = point.second - modelY;
		chi2 += diff * diff / modelY;
	}
	std::cout << "par0 = " << par[0] << "  chi2 = " << chi2 << std::endl;
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
