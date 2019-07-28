#ifndef HCMR_DATA_H
#define HCMR_DATA_H

#include "HCMRSpectrum.h"
#include "IPlotter.h"

class HCMRData
{
public:
	HCMRData();
	const HCMRSpectrum& getSpectum() const;
	void print() const;
	void fillDataFromSpeFile(const std::string& file);
	void findPeeks();
	static void setPlotter(Plotter* plotter);
	static void removePlotter();
public:
	HCMRSpectrum _spectrum;
	bool _isCalibrated;
	uint16_t _numOfChanels;
	double _keVperChannel;
	double _offsetInKeV;
	std::string _dateCollected;
	std::string _timeCollected;
	uint16_t _liveMeassurementDuration; //sec
	uint16_t _realMeassurementDuration; //sec
	std::vector<std::string> _sourceFileInfo;
	static Plotter* fplotter;
};

#endif // HCMR_DATA_H
