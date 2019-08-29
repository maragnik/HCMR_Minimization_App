#ifndef PLOT_SPECTRUM_H
#define PLOT_SPECTRUM_H
#include "HCMRSpectrum.h"
#include "qcustomplot.h"
#include <vector>
#include "IPlotter.h"
#include "HCMRPeekFinder.h"

class QTPlotter : public Plotter
{
public:
	QTPlotter();
	QTPlotter(QCustomPlot* customPlotData);

	void plot(const HCMRSpectrum& spectrum, int graphNum);
	void plot(std::vector<double> vector, int startChannel, int graphNum) override;
	void plot(std::vector<double> vector, std::vector<int> x, std::vector<int> y, int graphNum) override;
	void plotPeeks(std::vector<HCMRPeek> peeks);
	void plotFullPeeks(const std::vector<double>& data, std::vector<HCMRPeek> peeks);
	void plotRowData(std::vector<double> data, int graphNumber);

	void setUpForPeekPlot();
	void setAxisLabels(QString xlabel, QString yLabel);

	void setGraph(QCustomPlot* customPlotData);

private:
	QCustomPlot* _customPlot;
	int _currentGraph;
	bool _shouldScale;

};
#endif //PLOT_SPECTRUM_H
