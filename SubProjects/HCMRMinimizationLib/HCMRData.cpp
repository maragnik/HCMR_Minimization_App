#include "HCMRData.h"

HCMRData::HCMRData() : _data()
{
	_data.fillFromFile("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testRandom.txt");
}

const HCMRSpectrum& HCMRData::getSpectum() const
{
	return _data;
}
