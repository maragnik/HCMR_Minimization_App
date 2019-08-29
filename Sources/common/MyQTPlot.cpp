#include "MyQTPlot.h"
#include <algorithm>

MyQTPlot::MyQTPlot(QCustomPlot* customPlotData) :
	_customPlot(customPlotData),
	_currentGraph(0),
	_shouldScale(true)
{}

MyQTPlot::MyQTPlot() :
	_customPlot(nullptr),
	_currentGraph(0),
	_shouldScale(true)
{

}
void MyQTPlot::plot(const HCMRSpectrum& spectrum, int graphNum)
{
	plot(spectrum.getDataVector(), 0, graphNum);
}

void MyQTPlot::plotPeeks(std::vector<HCMRPeek> peeks)
{
	QVector<double> y;
	QVector<double> x;
	for (int i = 0; i < peeks.size(); ++i)
	{
		x.push_back(peeks[i].channel);
		y.push_back(peeks[i].value);
	}
	_customPlot->graph(_currentGraph)->setData(x, y);
	_customPlot->replot();
	QApplication::processEvents();

}
void MyQTPlot::plotFullPeeks(const std::vector<double>& data, std::vector<HCMRPeek> peeks)
{
	QVector<double> y;
	QVector<double> x;
	for (int i = 1; i < peeks.size(); ++i)
	{
		int minJ = std::max(static_cast<int>(peeks[i].channel) - peeks[i].widthPtV / 2, 0);
		int maxJ = std::min(static_cast<int>(peeks[i].channel) + peeks[i].widthPtV / 2, static_cast<int>(data.size() - 1));
		for (int j = minJ; j <= maxJ; ++j)
		{
			x.push_back(j);
			y.push_back(data[j]);
		}

	}
	_customPlot->graph(_currentGraph)->setData(x, y);
	_customPlot->replot();
	QApplication::processEvents();
}

void MyQTPlot::plotRowData(std::vector<double> data, int graphNumber)
{
	int numOfGraphs = _customPlot->graphCount();
	while (numOfGraphs < graphNumber + 1)
	{
		_customPlot->addGraph();
		numOfGraphs++;
	}
	setAxisLabels("Energy Channel", "Counts");
	_currentGraph = graphNumber;
	_shouldScale = true;
	_customPlot->graph(_currentGraph)->setLineStyle(QCPGraph::lsLine);
	_customPlot->graph(_currentGraph)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssNone));
	_customPlot->graph(_currentGraph)->setPen(QPen(Qt::black));

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

	_customPlot->graph(_currentGraph)->setData(x, y);
	if (_shouldScale)
	{
		_customPlot->xAxis->setRange(minXValue, maxXValue);
		_customPlot->yAxis->setRange(minYValue, maxYValue + maxYValue * 0.1);
	}
	setInitialPlotRange(_customPlot->xAxis->range(), _customPlot->yAxis->range());

	_customPlot->replot();
	QApplication::processEvents();
}

void MyQTPlot::setUpForPeekPlot()
{
	_currentGraph = 1;
	_shouldScale = false;
	_customPlot->graph(_currentGraph)->setLineStyle(QCPGraph::lsNone);
	_customPlot->graph(_currentGraph)->setScatterStyle(QCPScatterStyle(QCPScatterStyle::ssCircle, Qt::red, Qt::red, 7));
	_customPlot->replot();
}
void MyQTPlot::setAxisLabels(QString xlabel, QString yLabel)
{
	_customPlot->xAxis->setLabel(xlabel);
	_customPlot->yAxis->setLabel(yLabel);
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
	QApplication::processEvents();
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
