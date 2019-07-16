#ifndef HCMRSPECTRUM_H
# define HCMRSPECTRUM_H

#include "common_definitions.h"
#include <fstream>
#include <iostream>
#include <limits>

class HCMRSpectrum
{
public:
	HCMRSpectrum();
	void addPoint(XType x, YType y);
	const YType getYViaLinearInterpolation(XType x) const;
	void setName(std::string name);
	void print() const;
	bool fillFromFile(const std::string& file);
	void fillNameFromFile(const std::string& file);
	const XYMapType& getXYData() const;

private:
	XYMapType _spectrum;
	std::string _name;
	bool _isEmpty;
};

#endif // HCMRSPECTRUM_H
