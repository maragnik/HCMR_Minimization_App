#include "HCMRData.h"
#include "HCMRParser.h"
#include "loguru.h"

HCMRData::HCMRData() : _spectrum() {}

const HCMRSpectrum& HCMRData::getSpectum() const
{
	return _spectrum;
}

void HCMRData::setPlotter(Plotter* plotter)
{
	fplotter = plotter;
}

void HCMRData::removePlotter()
{
	fplotter = nullptr;
}

Plotter* HCMRData::fplotter = nullptr;

void HCMRData::print() const
{
	printf("=============================START OF DATA PRINTING=================================\n");
	printf("Printing Data initiated from spectrum with name: %s\n", _spectrum.getName().c_str());
	if (_sourceFileInfo.size() == 3)
	{
		printf("Source file name is------------------------: %s\n", (_sourceFileInfo[0] + _sourceFileInfo[1]).c_str());
		printf("It is located in path----------------------: %s\n", (_sourceFileInfo[2]).c_str());
	}
	else if (_sourceFileInfo.size() == 2)
	{
		printf("Source file name is------------------------: %s\n", (_sourceFileInfo[0] + _sourceFileInfo[1]).c_str());
	}
	else
	{
		printf("Source file name is------------------------: %s\n", (_sourceFileInfo[0]).c_str());
	}

	printf("-----------------------------------------------------------------------------------\n");
	if (_isCalibrated)
	{
		printf("Printing Data are allready calibrated\n");
		printf("keV per channel: %f\n", _keVperChannel);
		printf("Offset in keV  : %f\n", _offsetInKeV);
	}
	else
	{
		printf("Printing Data are not yet calibrated\n");
	}
	printf("Number of energy channels got from .spe file: %d\n", _numOfChanels);
	printf("Number of energy channels stored in vector--: %d\n", _spectrum.size());
	printf("Data collected on date----------------------: %s\n", _dateCollected.c_str());
	printf("Data collected on time----------------------: %s\n", _timeCollected.c_str());
	printf("Measurement duration (Live)-----------------: %d\n", _liveMeassurementDuration);
	printf("Measurement duration (Real)-----------------: %d\n", _realMeassurementDuration);
	_spectrum.print();
	printf("=============================END OF DATA PRINTING==================================\n");
}

void HCMRData::fillDataFromSpeFile(const std::string & file)
{
	HCMRParser parser;
	parser.parseSpeDataFile(file, this);
	_sourceFileInfo = parser.splitPath(file);
	_spectrum.setName(_sourceFileInfo.at(0));
}

void HCMRData::findPeeks()
{

}
