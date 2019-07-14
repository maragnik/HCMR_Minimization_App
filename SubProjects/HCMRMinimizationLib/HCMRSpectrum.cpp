#include "HCMRSpectrum.h"
#include <algorithm>


HCMRSpectrum::HCMRSpectrum() {}

void HCMRSpectrum::addPoint(typeX x, typeY y)
{
	_spectrum.insert(std::pair<typeX, typeY>(x, y));
}

const typeY HCMRSpectrum::getYViaLinearInterpolation(typeX x) const
{
	typeY ret = 0;
	if (x < _spectrum.begin()->first)
	{
		ret = _spectrum.begin()->second;
	}
	else if (x > _spectrum.rbegin()->first)
	{
		ret = _spectrum.rbegin()->second;
	}
	else
	{
		DataType::const_iterator it = _spectrum.lower_bound(x);
		typeX xUp = it->first;
		typeX yUp = it->second;

		if (xUp - x < LOW_VALUE)
		{
			ret = it->second;
		}
		else
		{
			it--;
			typeX xDown = it->first;
			typeX yDown = it->second;
			ret = static_cast<typeY>(yDown + (x - xDown) * (yUp - yDown) / static_cast<double>(xUp - xDown));
		}

	}
	return ret;

}

void HCMRSpectrum::setName(std::string name)
{
	_name = name;
}

void HCMRSpectrum::print() const
{

	std::cout << "UnitarySpectrumName = " << _name << std::endl;
	std::cout << "-----------------------------------------------" << std::endl;
	for (PointType point : _spectrum)
	{
		std::cout << "X = " << point.first << "   Y = " << point.second << std::endl;
	}
}

bool HCMRSpectrum::fillFromFile(const std::string & file)
{
	std::ifstream inFile(file);

	if (!inFile)
	{
		std::cerr << "Could not open file " << file << std::endl;
		return false;
	}

	fillNameFromFile(file);
	_spectrum.clear();

	// read until you reach the end of the file
	std::string line;
	while (std::getline(inFile, line))
	{
		size_t delimeterPosition = std::string::npos;
		size_t pos = std::string::npos;
		line.erase(0, line.find_first_not_of(" "));
		line.erase(0, line.find_first_not_of("\t"));

		delimeterPosition = line.find(" ");
		pos = line.find("\t");
		if (pos < delimeterPosition) delimeterPosition = pos;
		pos = line.find(",");
		if (pos < delimeterPosition) delimeterPosition = pos;
		pos = line.find("-");
		if (pos < delimeterPosition) delimeterPosition = pos;

		std::string srtValueX = line.substr(0, delimeterPosition);
		std::string srtValueY = line.substr(delimeterPosition + 1);

		double valueX = std::stod(srtValueX);
		double valueY = std::stod(srtValueY);
		addPoint(valueX, valueY);
	}
}

void HCMRSpectrum::fillNameFromFile(const std::string & file)
{
	size_t pos = file.find_last_of("\\");
	size_t posExt = file.find_last_of(".");
	if (file.empty() || (pos == std::string::npos))
	{
		_name = "Undefined";
		return;
	}

	if (pos < posExt)
	{
		_name = file.substr(pos + 1, posExt - pos - 1);
	}
	else
	{
		_name = file.substr(pos + 1);
	}
}

const DataType& HCMRSpectrum::getXYData() const
{
	return _spectrum;
}
