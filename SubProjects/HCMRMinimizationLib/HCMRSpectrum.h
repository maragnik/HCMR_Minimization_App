#ifndef HCMRSPECTRUM_H
# define HCMRSPECTRUM_H

#include <map>
#include <string>
#include <fstream>
#include <iostream>
#include <limits>

using typeX = double;
using typeY = double;
using DataType = std::map<typeX, typeY>;
using PointType = std::pair<typeX, typeY>;
const double LOW_VALUE = std::numeric_limits<float>::min();

class HCMRSpectrum
{
public:
	HCMRSpectrum();
	void addPoint(typeX x, typeY y);
	const typeY getYViaLinearInterpolation(typeX x) const;
	void setName(std::string name);
	void print() const;
	bool fillFromFile(const std::string& file);
	void fillNameFromFile(const std::string& file);
	const DataType& getXYData() const;

private:
	DataType _spectrum;
	std::string _name;
};

#endif // HCMRSPECTRUM_H
