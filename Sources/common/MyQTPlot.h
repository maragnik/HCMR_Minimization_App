#ifndef MY_QTPLOT_H
#define MY_QTPLOT_H
#include "HCMRSpectrum.h"
#include "qcustomplot.h"
#include <vector>
#include "IPlotter.h"
#include "HCMRPeekFinder.h"

class MyQTPlot : public Plotter
{
public:
	MyQTPlot();
	MyQTPlot(QCustomPlot* customPlotData);

	void plot(const HCMRSpectrum& spectrum, int graphNum);
	void plot(std::vector<double> vector, int startChannel, int graphNum) override;
	void plot(std::vector<double> vector, std::vector<int> x, std::vector<int> y, int graphNum) override;
	void plotPeeks(std::vector<HCMRPeek> peeks);
	void plotFullPeeks(const std::vector<double>& data, std::vector<HCMRPeek> peeks);
	void plotRowData(std::vector<double> data, int graphNumber);

	void setUpForPeekPlot();
	void setAxisLabels(QString xlabel, QString yLabel);

	void setCustomPlot(QCustomPlot* customPlotData);
	QCustomPlot* getCustomPlot();

public:
	void setInitialPlotRange(QCPRange xRange, QCPRange yRange);
	void resetInitialPlotRange();
	QCPRange getInitialPlotXRange();
	QCPRange getInitialPlotYRange();

private:
	QCustomPlot* _customPlot;
	QCPRange _intialPlotXRange;
	QCPRange _intialPlotYRange;
	int _currentGraph;
	bool _shouldScale;

};
#endif //MY_QTPLOT_H
