#ifndef HCMRdata__H
#define HCMRdata__H

#include "HCMRSpectrum.h"
#include "IPlotter.h"

class HCMRData
{
public:
	HCMRData();
	const HCMRSpectrum& getSpectum() const;
	void print() const;
	void fillDataFromSpeFile(const std::string& file);
	void findPeaks();
	static void setPlotter(Plotter* plotter);
	static void removePlotter();
public:
	long long _id;
	HCMRSpectrum _spectrum;
	bool _isCalibrated;
	uint16_t _numOfChanels;
	double _keVperChannel;
	double _offsetInKeV;
	std::string _dateCollected;
	std::string _timeCollected;
	uint16_t _liveMeassurementDuration; //sec
	uint16_t _realMeassurementDuration; //sec
	std::string _filePath;
	std::string _fileName;

	static Plotter* fplotter;
private:
	static int nextID;
};

#endif // HCMRdata__H
