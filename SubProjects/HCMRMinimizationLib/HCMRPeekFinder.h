#ifndef HCMR_PEEKFINDER_H
#define HCMR_PEEKFINDER_H

#include "Minuit2/FCNBase.h"
#include "HCMRSpectrum.h"
#include "IPlotter.h"

class PeekGaussFCN : public ROOT::Minuit2::FCNBase
{
public:
	PeekGaussFCN();

	// FCNBase methods implementation
	double operator()(const std::vector<double>& par) const override;
	double Up() const override;
	void setData(const std::vector<double>& data);

private:
	const std::vector<double>* _data;

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

	PeekGaussFCN _fcn;
	std::vector<int> _finalPeekIndexes;
	std::vector<int> _finalPeekWidths;
private:
	const std::vector<double>* _data;

	//Peek data
	std::vector<int> _peekIndexes;
	std::vector<int> _maxPeekWindow;
	std::vector<int> _maxPeekWindowNoEdge;
	std::vector<double> _maxPeekToValey;
	std::vector<double> _peekWidth;
};

#endif //HCMR_PICKFINDER_H
