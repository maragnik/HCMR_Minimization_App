#ifndef HCMRpeakFinder__H
#define HCMRpeakFinder__H

#include "Minuit2/FCNBase.h"
#include "HCMRSpectrum.h"
#include "IPlotter.h"

struct HCMRPeak
{
	uint32_t channel = 0;
	double value = 0;
	uint16_t widthNoEdge = 0;
	uint16_t width = 0;
	uint16_t widthPtV = 0;
	double peakToValey = 0.F;
	bool isValid = true;
};

class PeakGaussFCN : public ROOT::Minuit2::FCNBase
{
public:
	PeakGaussFCN();

	// FCNBase methods implementation
	double operator()(const std::vector<double>& par) const override;
	double Up() const override;
	void setData(const std::vector<double>& data);

private:
	const std::vector<double>* data_;

};

class HCMRPeakFinder
{
public:
	HCMRPeakFinder();
	void setData(const std::vector<double>& data);
	void findPeaks();
	void findAllPeaks(const std::vector<double>& data);
	void correctWidthNoEdgeValues();
	void choosePeaks(int minPeakWindow, int minPeakWindowNoEdges, double minPeakToValey, int minPeakWidth);
	bool isPeak(const std::vector<double>& data, int index);
	double getPeakToValey(const std::vector<double>& data, int peakIndex, int halfWindow, int& peakWidth);
	int peakFound(const std::vector<double>& data, int centerIndex, int halfWindow);
	bool peakRemains(const std::vector<double>& data, int peakIndex, int halfWindow, bool accountForEdges);
	std::vector<double> makeLogarithmic(const std::vector<double>& data);
	std::vector<double> smooth(const std::vector<double>& data, int width);

	PeakGaussFCN fcn_;
	std::vector<HCMRPeak> finalPeaks_;

private:
	const std::vector<double>* data_;

	//Peak data
	std::vector<HCMRPeak> peaks_;
};

#endif //HCMR_PICKFINDER_H
