#include "HCMRPeakFinder.h"
#include "HCMRData.h"
#include <math.h>  
#include <algorithm>
#include "Minuit2/MnMigrad.h"
#include "Minuit2/FunctionMinimum.h"
#include "Minuit2/MnPrint.h"
#include <time.h>

HCMRPeakFinder::HCMRPeakFinder() : data_(nullptr), fcn_()
{}

void HCMRPeakFinder::setData(const std::vector<double>& data)
{
	allPeaks_.clear();
	finalPeaks_.clear();
	data_ = &data;
}

void HCMRPeakFinder::findPeaks()
{

	std::vector<double> logData = makeLogarithmic(*data_);
	std::vector<double> smoothLogData = smooth(logData, 10);

	findAllPeaks(smoothLogData);
	//choosePeaks(21, 21, 0.1);

}

bool HCMRPeakFinder::isPeak(const std::vector<double>& data, int index)
{
	bool is_peak = false;
	if (index != 0 && index != data.size() - 1)
	{
		if (data[index - 1] < data[index] && data[index + 1] < data[index])
		{
			is_peak = true;
		}
	}
	return is_peak;
}

int HCMRPeakFinder::peakFound(const std::vector<double>& data, int centerIndex, int halfWindow)
{
	int minJ = std::max(centerIndex - halfWindow, 0);
	int maxJ = std::min(centerIndex + halfWindow, static_cast<int>(data.size() - 1));
	double peakValue = data.at(minJ);
	int peakIndex = minJ;
	for (int i = minJ + 1; i <= maxJ; ++i)
	{
		if (data.at(i) > peakValue)
		{
			peakValue = data.at(i);
			peakIndex = i;
		}
	}

	if (peakIndex != minJ && peakIndex != maxJ)
	{
		return peakIndex;
	}
	else
	{
		return -1;
	}
}

bool HCMRPeakFinder::peakRemains(const std::vector<double>& data, int peakIndex, int halfWindow, bool accountForEdges)
{
	int minJ = std::max(peakIndex - halfWindow, 0);
	int maxJ = std::min(peakIndex + halfWindow, static_cast<int>(data.size() - 1));
	int newPeakIndex = peakFound(data, peakIndex, halfWindow);

	if ((newPeakIndex == peakIndex) || (!accountForEdges && newPeakIndex == -1))
	{
		return true;
	}
	else
	{
		return false;
	}
}

void HCMRPeakFinder::choosePeaks(int minPeakWindow, int minPeakWindowNoEdges, double minPeakToValey, int minPeakWidth)
{
	finalPeaks_.clear();
	for (int i = 0; i < allPeaks_.size(); ++i)
	{
		if (allPeaks_[i].width >= minPeakWindow &&
			allPeaks_[i].widthNoEdge >= minPeakWindowNoEdges &&
			allPeaks_[i].peakToValey * 100. / allPeaks_[i].value >= minPeakToValey &&
			allPeaks_[i].widthPtV >= minPeakWidth)
		{
			finalPeaks_.push_back(allPeaks_[i]);
		}
	}
}

void HCMRPeakFinder::findAllPeaks(const std::vector<double>& data)
{
	allPeaks_.clear();

	bool accoundForEdges = true;
	for (int peakIndex = 1; peakIndex < data.size() - 1; ++peakIndex)
	{
		if (isPeak(data, peakIndex))
		{
			HCMRPeak peak;
			peak.channel = peakIndex;
			peak.value = data[peakIndex];
			int currentHalfWindow = 2;
			while (peakRemains(data, peakIndex, currentHalfWindow, accoundForEdges) && (2 * currentHalfWindow + 1 < data.size()))
			{
				currentHalfWindow++;
			}
			peak.width = 2 * (currentHalfWindow - 1) + 1;
			int peakwith;
			peak.peakToValey = getPeakToValey(data, peakIndex, currentHalfWindow - 1, peakwith);
			peak.widthPtV = peakwith;

			while (peakRemains(data, peakIndex, currentHalfWindow, !accoundForEdges) && (2 * currentHalfWindow + 1 < data.size()))
			{
				currentHalfWindow++;
			}
			peak.widthNoEdge = 2 * (currentHalfWindow - 1) + 1;
			allPeaks_.push_back(peak);
		}
	}
	correctWidthNoEdgeValues();
}

void HCMRPeakFinder::correctWidthNoEdgeValues()
{
	for (int i = 0; i < allPeaks_.size(); ++i)
	{
		int j = i + 1;
		while ((j != allPeaks_.size()) && (allPeaks_[j].channel < (allPeaks_[i].channel + allPeaks_[i].widthNoEdge / 2)))
		{
			if (allPeaks_[j].value > allPeaks_[i].value)
			{
				allPeaks_[i].widthNoEdge = 2 * (allPeaks_[j].channel - allPeaks_[i].channel - 1) + 1;
			}
			j++;
		}
	}

}

std::vector<double> HCMRPeakFinder::makeLogarithmic(const std::vector<double> & data)
{
	std::vector<double> returnData;
	for (double value : data)
	{
		returnData.push_back(log10(value));
	}
	return returnData;
}

std::vector<double> HCMRPeakFinder::smooth(const std::vector<double> & data, int width)
{
	std::vector<double> returnData;
	for (int i = 0; i < data.size(); ++i)
	{
		int maxJ = std::min(i + width / 2, static_cast<int>(data.size() - 1));
		int minJ = std::max(i - width / 2, 0);
		double sum = 0;
		for (int j = minJ; j <= maxJ; ++j)
		{
			sum += data.at(j);
		}
		sum = (sum / (maxJ - minJ + 1));
		returnData.push_back(sum);
	}
	return returnData;
}

double HCMRPeakFinder::getPeakToValey(const std::vector<double> & data, int peakIndex, int halfWindow, int& peakWidth)
{
	double peakValue = data[peakIndex];
	double minValueLeft = peakValue;
	double minValueRight = peakValue;
	int minIndexLeft = peakIndex;
	int minIndexRight = peakIndex;
	int minJ = std::max(peakIndex - halfWindow, 0);
	int maxJ = std::min(peakIndex + halfWindow, static_cast<int>(data.size() - 1));
	for (int i = minJ; i < peakIndex; ++i)
	{
		if (minValueLeft > data[i])
		{
			minIndexLeft = i;
			minValueLeft = data[i];
		}
	}
	for (int i = peakIndex + 1; i <= maxJ; ++i)
	{
		if (minValueRight > data[i])
		{
			minIndexRight = i;
			minValueRight = data[i];
		}
	}

	double minValue = minValueRight;
	peakWidth = 2 * (minIndexRight - peakIndex) + 1;
	if (minValueLeft > minValueRight)
	{
		minValue = minValueLeft;
		peakWidth = 2 * (peakIndex - minIndexLeft) + 1;
	}
	return peakValue - minValue;
}

/******************************************************************************************************
 *         MINUIT FCN DEFINITIONS                                                                     *
 ******************************************************************************************************/



PeakGaussFCN::PeakGaussFCN() : data_(nullptr)
{}

void PeakGaussFCN::setData(const std::vector<double> & data)
{
	data_ = &data;
}
// FCNBase methods implementation
double PeakGaussFCN::operator()(const std::vector<double> & par) const
{
	//double yGauss = par[2] * std::exp(-(x - _offset + par[0]) * (x - _offset + par[0]) / (2 * (par[1] / 2.355) * (par[1] / 2.355))) + par[3];
	return 0;
}

double PeakGaussFCN::Up() const
{
	return 1.;
}
