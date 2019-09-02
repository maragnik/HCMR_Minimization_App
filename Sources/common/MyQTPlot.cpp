#include "MyQTPlot.h"
#include <algorithm>

MyQTPlot::MyQTPlot(QCustomPlot* customPlotData) :
	_customPlot(customPlotData),
	_shouldScale(true)
{}

MyQTPlot::MyQTPlot() :
	_customPlot(nullptr),
	_shouldScale(true)
{

}
void MyQTPlot::plot(const HCMRSpectrum& spectrum, int graphNum)
{
	plot(spectrum.getDataVector(), 0, graphNum);
}

void MyQTPlot::checkAddGraph(int graphNumber)
{
	int numOfGraphs = _customPlot->graphCount();
	while (numOfGraphs < graphNumber + 1)
	{
		_customPlot->addGraph();
		numOfGraphs++;
	}
}

void MyQTPlot::plotRowData(std::vector<double> data, int graphNumber)
{
	checkAddGraph(graphNumber);
	_customPlot->xAxis->setLabel("Energy Channel");
	_customPlot->yAxis->setLabel("Counts");

	_shouldScale = true;
	_customPlot->graph(graphNumber)->setLineStyle(QCPGraph::lsLine);
	_customPlot->graph(graphNumber)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssNone));
	_customPlot->graph(graphNumber)->setPen(QPen(Qt::black));

	double maxYValue = 0;
	double minYValue = 20000;
	double minXValue = 0;
	double maxXValue = data.size();

	QVector<double> y = QVector<double>::fromStdVector(data);
	QVector<double> x;
	for (int i = minXValue; i < maxXValue; ++i)
	{
		maxYValue = (y[i] > maxYValue) ? y[i] : maxYValue;
		minYValue = (y[i] < minYValue && y[i] != 0) ? y[i] : minYValue;
		x.push_back(i);
	}

	_customPlot->graph(graphNumber)->setData(x, y);
	if (_shouldScale)
	{
		_customPlot->xAxis->setRange(minXValue, maxXValue);
		_customPlot->yAxis->setRange(minYValue, maxYValue + maxYValue * 0.1);
	}
	setInitialPlotRange(_customPlot->xAxis->range(), _customPlot->yAxis->range());

	_customPlot->replot();
	QApplication::processEvents();
}

void MyQTPlot::plotPeaks(std::vector<HCMRPeak> peaks, int graphNumber)
{
	checkAddGraph(graphNumber);
	_shouldScale = false;
	_customPlot->graph(graphNumber)->setLineStyle(QCPGraph::lsNone);
	_customPlot->graph(graphNumber)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssCircle, Qt::blue, Qt::blue, 7));

	QVector<double> y;
	QVector<double> x;
	for (int i = 0; i < peaks.size(); ++i)
	{
		x.push_back(peaks[i].channel);
		y.push_back(peaks[i].value);
	}
	_customPlot->graph(graphNumber)->setData(x, y);
	_customPlot->replot();
}

void MyQTPlot::plotSelectedPeak(HCMRPeak peak, int graphNumber)
{
	QVector<double> y;
	QVector<double> x;
	checkAddGraph(graphNumber);

	if (peak.isValid)
	{
		_shouldScale = false;
		_customPlot->graph(graphNumber)->setLineStyle(QCPGraph::lsNone);
		_customPlot->graph(graphNumber)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssCircle, Qt::blue, Qt::blue, 12));
		x.push_back(peak.channel);
		y.push_back(peak.value);
	}
	_customPlot->graph(graphNumber)->setData(x, y);
	_customPlot->replot();
}

void MyQTPlot::plotCalibrationPeaks(std::vector<PeakSearchConfigEntry> peakSearchConfigEntries)
{
	removeCalibrationsPeaks();
	for (int i = 0; i < peakSearchConfigEntries.size(); ++i)
	{
		QCPItemStraightLine* infLine = new QCPItemStraightLine(_customPlot);
		QPen pen;
		pen.setWidth(3);
		pen.setColor(QColor(0, 0, 0, 50));
		infLine->setPen(pen);
		infLine->point1->setCoords(peakSearchConfigEntries[i].centerChannel, 0);
		infLine->point2->setCoords(peakSearchConfigEntries[i].centerChannel, 1);
		_peakLines.push_back(infLine);

		int channelsToLeft = peakSearchConfigEntries[i].searchRelativeToOtherPeak ?
			peakSearchConfigEntries[i].numOfChannelsToTheLeft2 :
			peakSearchConfigEntries[i].numOfChannelsToTheLeft;

		int channelsToRight = peakSearchConfigEntries[i].searchRelativeToOtherPeak ?
			peakSearchConfigEntries[i].numOfChannelsToTheRight2 :
			peakSearchConfigEntries[i].numOfChannelsToTheRight;

		QCPItemRect* rect = new QCPItemRect(_customPlot);
		rect->topLeft->setCoords(peakSearchConfigEntries[i].centerChannel - channelsToLeft, _customPlot->yAxis->range().upper * 100);
		rect->bottomRight->setCoords(peakSearchConfigEntries[i].centerChannel + channelsToRight, 0);
		rect->setBrush(peakSearchConfigEntries[i].searchRelativeToOtherPeak ? QColor(0, 0, 255, 20) : QColor(0, 255, 0, 10));
		pen.setColor(QColor(0, 0, 0, 0));
		rect->setPen(pen);
		_peakRects.push_back(rect);
	}
	_customPlot->replot();
}

void MyQTPlot::removeCalibrationsPeaks()
{
	for (int i = 0; i < _peakLines.size(); ++i)
	{
		_customPlot->removeItem(_peakLines[i]);
		_customPlot->removeItem(_peakRects[i]);
	}
	_peakLines.clear();
	_peakRects.clear();
}

void MyQTPlot::highlightSelectedCalibrationPeak(int index)
{
	QPen linePen(QColor(0, 0, 0, 50));
	linePen.setWidth(3);
	QPen rectPen(QColor(0, 0, 0, 0));

	for (int i = 0; i < _peakLines.size(); ++i)
	{
		_peakLines[i]->setPen(linePen);
		_peakRects[i]->setPen(rectPen);
	}
	linePen.setColor(QColor(0, 0, 0, 100));
	rectPen.setColor(QColor(0, 0, 0, 100));
	_peakLines[index]->setPen(linePen);
	_peakRects[index]->setPen(rectPen);
	_customPlot->replot();

}

void MyQTPlot::plot(std::vector<double> vector, int startChannel, int graphNum)
{
	// generate some data:
	double maxValue = 0;
	double minValue = 20000;
	QVector<double> x, y; // initialize with entries 0..100
	int count = startChannel;
	for (double counts : vector)
	{
		x.push_back(count++);
		y.push_back(counts);
		maxValue = (maxValue > counts) ? maxValue : counts;
		minValue = (counts < minValue && counts != 0) ? counts : minValue;
	}
	if (_customPlot->graphCount() < graphNum + 1)
	{
		for (int i = _customPlot->graphCount(); i <= graphNum; ++i)
		{
			_customPlot->addGraph();
		}
	}
	else
	{
		//_customPlot->graph(graphNum)->
	}

	_customPlot->graph(graphNum)->setData(x, y);

	if (graphNum == 0)
	{
		_customPlot->xAxis->setLabel("x");
		_customPlot->yAxis->setLabel("y");
		//_customPlot->yAxis->setScaleType(QCPAxis::stLogarithmic);
		_customPlot->xAxis->setRange(0, vector.size());
		_customPlot->yAxis->setRange(minValue, maxValue);
		_customPlot->replot();
		_customPlot->repaint();
		_customPlot->update();
	}
	else
	{
		_customPlot->graph(graphNum)->setPen(QPen(Qt::red));
	}

	_customPlot->replot();
}

void MyQTPlot::plot(std::vector<double> vector, std::vector<int> x, std::vector<int> y, int graphNum)
{
	QVector<double> qx;
	QVector<double> qy;
	QVector<double> err;

	double maxVal = 0;
	for (int i = 0; i < x.size(); ++i)
	{
		qx.push_back(x[i]);
		qy.push_back(vector[x[i]]);
		err.push_back(y[i]);
		if (maxVal < y[i]) maxVal = y[i];
	}

	QCPErrorBars* errorBars = new QCPErrorBars(_customPlot->xAxis, _customPlot->yAxis);
	errorBars->setDataPlottable(_customPlot->graph(1));

	if (_customPlot->graphCount() < graphNum + 1)
	{
		for (int i = _customPlot->graphCount(); i <= graphNum; ++i)
		{
			_customPlot->addGraph();
		}
	}
	_customPlot->graph(graphNum)->setData(qx, qy);

	errorBars->setData(err);
	_customPlot->graph(graphNum)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssCircle, Qt::red, Qt::white, 7));
	_customPlot->graph(graphNum)->setLineStyle(QCPGraph::lsNone);
	_customPlot->graph(graphNum)->setPen(QPen(Qt::red));


	_customPlot->replot();
	QApplication::processEvents();


}

void MyQTPlot::setCustomPlot(QCustomPlot * customPlotData)
{
	_customPlot = customPlotData;
}

QCustomPlot* MyQTPlot::getCustomPlot()
{
	return _customPlot;
}

void MyQTPlot::setInitialPlotRange(QCPRange XRange, QCPRange YRange)
{
	_intialPlotXRange = XRange;
	_intialPlotYRange = YRange;
}

void MyQTPlot::resetInitialPlotRange()
{
	_customPlot->xAxis->setRange(_intialPlotXRange);
	_customPlot->yAxis->setRange(_intialPlotYRange);
	_customPlot->replot();
}

QCPRange MyQTPlot::getInitialPlotXRange()
{
	return _intialPlotXRange;
}

QCPRange MyQTPlot::getInitialPlotYRange()
{
	return _intialPlotYRange;
}
