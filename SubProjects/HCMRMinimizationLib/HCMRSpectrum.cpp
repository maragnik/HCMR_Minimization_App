#include "HCMRSpectrum.h"
#include <algorithm>

HCMRSpectrum::HCMRSpectrum() {}

void HCMRSpectrum::addPoint(XType x, YType y)
{
	_spectrum.insert(std::pair<XType, YType>(x, y));
}

const YType HCMRSpectrum::getYViaLinearInterpolation(XType x) const
{
	YType ret = 0;
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
		XYMapType::const_iterator it = _spectrum.lower_bound(x);
		XType xUp = it->first;
		XType yUp = it->second;

		if (xUp - x < LOW_VALUE)
		{
			ret = it->second;
		}
		else
		{
			it--;
			XType xDown = it->first;
			XType yDown = it->second;
			ret = static_cast<YType>(yDown + (x - xDown) * (yUp - yDown) / static_cast<double>(xUp - xDown));
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
	size_t pos = std::min(file.find_last_of("\\"), file.find_last_of("/"));
	size_t posExt = file.find_last_of(".");
	if (file.empty() || (pos == std::string::npos))
	{
		_name = "Undefined";
		return;
	}
	_name = (pos < posExt) ? file.substr(pos + 1, posExt - pos - 1) : file.substr(pos + 1);
}

const XYMapType & HCMRSpectrum::getXYData() const
{
	return _spectrum;
}
