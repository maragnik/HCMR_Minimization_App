#include "HCMRPeekFinder.h"
#include "HCMRData.h"
#include <math.h>  
#include <algorithm>
#include "Minuit2/MnMigrad.h"
#include "Minuit2/FunctionMinimum.h"
#include "Minuit2/MnPrint.h"
#include <time.h>

HCMRPeekFinder::HCMRPeekFinder() : _data(nullptr), _fcn()
{}

void HCMRPeekFinder::setData(const std::vector<double>& data)
{
	_peekIndexes.clear();
	_maxPeekWindow.clear();
	_maxPeekWindowNoEdge.clear();
	_maxPeekToValey.clear();
	_peekWidth.clear();

	_finalPeekIndexes.clear();
	_finalPeekWidths.clear();
	_data = &data;
}

void HCMRPeekFinder::findPeeks()
{

	std::vector<double> logData = makeLogarithmic(*_data);
	std::vector<double> smoothLogData = smooth(logData, 10);

	findAllPeaks(smoothLogData);
	//choosePeaks(21, 21, 0.1);

}

bool HCMRPeekFinder::isPeek(const std::vector<double>& data, int index)
{
	bool is_peek = false;
	if (index != 0 && index != data.size() - 1)
	{
		if (data[index - 1] < data[index] && data[index + 1] < data[index])
		{
			is_peek = true;
		}
	}
	return is_peek;
}

int HCMRPeekFinder::peekFound(const std::vector<double>& data, int centerIndex, int halfWindow)
{
	int minJ = std::max(centerIndex - halfWindow, 0);
	int maxJ = std::min(centerIndex + halfWindow, static_cast<int>(data.size() - 1));
	double peekValue = data.at(minJ);
	int peekIndex = minJ;
	for (int i = minJ + 1; i <= maxJ; ++i)
	{
		if (data.at(i) > peekValue)
		{
			peekValue = data.at(i);
			peekIndex = i;
		}
	}

	if (peekIndex != minJ && peekIndex != maxJ)
	{
		return peekIndex;
	}
	else
	{
		return -1;
	}
}

bool HCMRPeekFinder::peekRemains(const std::vector<double>& data, int peekIndex, int halfWindow, bool accountForEdges)
{
	int minJ = std::max(peekIndex - halfWindow, 0);
	int maxJ = std::min(peekIndex + halfWindow, static_cast<int>(data.size() - 1));
	int newPeekIndex = peekFound(data, peekIndex, halfWindow);

	if ((newPeekIndex == peekIndex) || (!accountForEdges && newPeekIndex == -1))
	{
		return true;
	}
	else
	{
		return false;
	}
}

void HCMRPeekFinder::choosePeaks(int minPeekWindow, int minPeekWindowNoEdges, double minPeekToValey, int minPeekWidth)
{
	_finalPeekIndexes.clear();
	_finalPeekWidths.clear();
	for (int i = 0; i < _peekIndexes.size(); ++i)
	{
		if (_maxPeekWindow[i] >= minPeekWindow &&
			_maxPeekWindowNoEdge[i] >= minPeekWindowNoEdges &&
			_maxPeekToValey[i] >= minPeekToValey &&
			_peekWidth[i] >= minPeekWidth)
		{
			printf("%d\n", _peekIndexes[i]);
			_finalPeekIndexes.push_back(_peekIndexes[i]);
			_finalPeekWidths.push_back(_peekWidth[i]);
		}
	}
}

void HCMRPeekFinder::findAllPeaks(const std::vector<double>& data)
{
	_peekIndexes.clear();
	_maxPeekWindow.clear();
	_maxPeekWindowNoEdge.clear();
	_maxPeekToValey.clear();
	_peekWidth.clear();


	bool accoundForEdges = true;
	for (int peekIndex = 1; peekIndex < data.size() - 1; ++peekIndex)
	{
		if (isPeek(data, peekIndex))
		{
			_peekIndexes.push_back(peekIndex);

			int currentHalfWindow = 2;
			while (peekRemains(data, peekIndex, currentHalfWindow, accoundForEdges) && (2 * currentHalfWindow + 1 < data.size()))
			{
				currentHalfWindow++;
			}
			_maxPeekWindow.push_back(2 * (currentHalfWindow - 1) + 1);
			int peekwith;
			_maxPeekToValey.push_back(getPeekToValey(data, peekIndex, currentHalfWindow - 1, peekwith));
			_peekWidth.push_back(peekwith);

			while (peekRemains(data, peekIndex, currentHalfWindow, !accoundForEdges) && (2 * currentHalfWindow + 1 < data.size()))
			{
				currentHalfWindow++;
			}
			_maxPeekWindowNoEdge.push_back(2 * (currentHalfWindow - 1) + 1);
		}
	}
}

std::vector<double> HCMRPeekFinder::makeLogarithmic(const std::vector<double> & data)
{
	std::vector<double> returnData;
	for (double value : data)
	{
		returnData.push_back(log10(value));
	}
	return returnData;
}

std::vector<double> HCMRPeekFinder::smooth(const std::vector<double> & data, int width)
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

double HCMRPeekFinder::getPeekToValey(const std::vector<double> & data, int peekIndex, int halfWindow, int& peekWidth)
{
	double peekValue = data[peekIndex];
	double minValueLeft = peekValue;
	double minValueRight = peekValue;
	int minIndexLeft = peekIndex;
	int minIndexRight = peekIndex;
	int minJ = std::max(peekIndex - halfWindow, 0);
	int maxJ = std::min(peekIndex + halfWindow, static_cast<int>(data.size() - 1));
	for (int i = minJ; i < peekIndex; ++i)
	{
		if (minValueLeft > data[i])
		{
			minIndexLeft = i;
			minValueLeft = data[i];
		}
	}
	for (int i = peekIndex + 1; i <= maxJ; ++i)
	{
		if (minValueRight > data[i])
		{
			minIndexRight = i;
			minValueRight = data[i];
		}
	}

	double minValue = minValueRight;
	peekWidth = minIndexRight - peekIndex;
	if (minValueLeft > minValueRight)
	{
		minValue = minValueLeft;
		peekWidth = peekIndex - minIndexLeft;
	}
	return peekValue - minValue;
}

/******************************************************************************************************
 *         MINUIT FCN DEFINITIONS                                                                     *
 ******************************************************************************************************/



PeekGaussFCN::PeekGaussFCN() : _data(nullptr)
{}

void PeekGaussFCN::setData(const std::vector<double> & data)
{
	_data = &data;
}
// FCNBase methods implementation
double PeekGaussFCN::operator()(const std::vector<double> & par) const
{
	//double yGauss = par[2] * std::exp(-(x - _offset + par[0]) * (x - _offset + par[0]) / (2 * (par[1] / 2.355) * (par[1] / 2.355))) + par[3];
	return 0;
}

double PeekGaussFCN::Up() const
{
	return 1.;
}
