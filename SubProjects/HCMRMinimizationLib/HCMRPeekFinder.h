#ifndef HCMR_PEEKFINDER_H
#define HCMR_PEEKFINDER_H

#include "Minuit2/FCNBase.h"
#include "HCMRSpectrum.h"
#include "IPlotter.h"

struct HCMRPeek
{
	uint32_t channel = 0;
	uint16_t widthNoEdge = 0;
	uint16_t width = 0;
	uint16_t widthPtV = 0;
	double peekToValey = 0.F;
};

class PeekGaussFCN : public ROOT::Minuit2::FCNBase
{
public:
	PeekGaussFCN();

	// FCNBase methods implementation
	double operator()(const std::vector<double>& par) const override;
	double Up() const override;
	void setData(const std::vector<double>& data);

private:
	const std::vector<double>* data_;

};

class HCMRPeekFinder
{
public:
	HCMRPeekFinder();
	void setData(const std::vector<double>& data);
	void findPeeks();
	void findAllPeaks(const std::vector<double>& data);
	void choosePeaks(int minPeekWindow, int minPeekWindowNoEdges, double minPeekToValey, int minPeekWidth);
	bool isPeek(const std::vector<double>& data, int index);
	double getPeekToValey(const std::vector<double>& data, int peekIndex, int halfWindow, int& peekWidth);
	int peekFound(const std::vector<double>& data, int centerIndex, int halfWindow);
	bool peekRemains(const std::vector<double>& data, int peekIndex, int halfWindow, bool accountForEdges);
	std::vector<double> makeLogarithmic(const std::vector<double>& data);
	std::vector<double> smooth(const std::vector<double>& data, int width);

	PeekGaussFCN fcn_;
	std::vector<HCMRPeek> finalPeeks_;

private:
	const std::vector<double>* data_;

	//Peek data
	std::vector<HCMRPeek> peeks_;
};

#endif //HCMR_PICKFINDER_H
