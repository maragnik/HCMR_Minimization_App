#include "HCMRModel.h"
//#include <iostream>

HCMRModel::HCMRModel() {}

void HCMRModel::addNewUnitary(const std::string& file)
{
	HCMRSpectrum unitary;
	unitary.fillFromFile(file);
	_unitaries.push_back(unitary);
}

void HCMRModel::print() const
{
	std::cout << "\n\n########################################################" << std::endl;
	std::cout << "PRINTING MODEL CONSISTING OF " << _unitaries.size() << " UNITARIES      " << std::endl;
	std::cout << "########################################################\n" << std::endl;
	int count = 0;
	for (HCMRSpectrum unitary : _unitaries)
	{
		count++;
		std::cout << "Unitary #" << count << std::endl;
		unitary.print();
	}
}

YType HCMRModel::getY(const std::vector<double>& par, XType x) const
{
	YType ret = 0;
	for (int i = 0; i < _unitaries.size(); ++i)
	{
		const HCMRSpectrum& spectrum = _unitaries.at(i);
		ret += spectrum.getYViaLinearInterpolation(x) * par[0];
	}
	return ret;
}
