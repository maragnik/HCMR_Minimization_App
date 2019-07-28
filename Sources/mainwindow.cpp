﻿#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "HCMRParser.h"
#include "QTPlotter.h"
#include "HCMRPeekFinder.h"
#include "loguru.h"

MainWindow::MainWindow(QWidget* parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	ui->setupUi(this);
	ui->customPlot->addGraph();
	ui->customPlot->addGraph();
	ui->customPlot->setInteraction(QCP::iRangeDrag, true);
	ui->customPlot->setInteraction(QCP::iRangeZoom, true);
	connect(ui->button_browse_data, &QAbstractButton::clicked, this, &MainWindow::browseForDataFile);
	connect(ui->checkbox_yAxisScaleType, SIGNAL(toggled(bool)), this, SLOT(setDataYAxisScaleType(bool)));
	connect(ui->spinBox_minPeekRange, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->spinBox_minPeekRangeNoEdge, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->spinBox_minPeekToValey, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->spinBox_minPeekWidth, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
}

MainWindow::~MainWindow()
{
	delete ui;
}

void MainWindow::myplot()
{
	_plotter.setGraph(ui->customPlot);

	_plotter.setAxisLabels("Energy Channel", "Counts");
	_plotter.setUpForRowDataPlot();
	_plotter.plotRowData(_data.getSpectum().getDataVector());

	_peekFinder.setData(_data.getSpectum().getDataVector());
	_peekFinder.findAllPeaks(_data.getSpectum().getDataVector());
	std::vector<double> data = _data.getSpectum().getDataVector();
	//std::vector<double> logData = peekFinder.makeLogarithmic(_data.getSpectum().getDataVector());
	//std::vector<double> smoothLogData = peekFinder.smooth(logData, 10);

	_peekFinder.findAllPeaks(data);
	peekConfigChanged(0);
}

void MainWindow::browseForDataFile()
{
	QString fileName = QFileDialog::getOpenFileName(this, ("Open Spectrum File"),
		QDir::currentPath(),
		("Spectra (*.spe)"));
	if (!fileName.isEmpty()) {
		_data.fillDataFromSpeFile(fileName.toStdString());
		_data.print();
		myplot();
	}
}

void MainWindow::setDataYAxisScaleType(bool isChecked)
{
	if (!isChecked)
		ui->customPlot->yAxis->setScaleType(QCPAxis::stLinear);
	else
		ui->customPlot->yAxis->setScaleType(QCPAxis::stLogarithmic);

	ui->customPlot->replot();

}

void MainWindow::peekConfigChanged(int val)
{
	int minPeekRange = ui->spinBox_minPeekRange->value();
	int minPeekRangeNoEdge = ui->spinBox_minPeekRangeNoEdge->value();
	double minPeekToValey = static_cast<double>(ui->spinBox_minPeekToValey->value());
	int minPeekWidth = ui->spinBox_minPeekWidth->value();

	_peekFinder.choosePeaks(minPeekRange, minPeekRangeNoEdge, minPeekToValey, minPeekWidth);
	ui->customPlot->addGraph();
	_plotter.setUpForPeekPlot();
	_plotter.plotPeeksToData(_data.getSpectum().getDataVector(), _peekFinder._finalPeekIndexes);

	ui->list_peeks->clear();
	for (int i = 0; i < _peekFinder._finalPeekIndexes.size(); ++i)
	{
		ui->list_peeks->addItem(QString::number(_peekFinder._finalPeekIndexes[i]) + "  " + QString::number(_peekFinder._finalPeekWidths[i]));
	}
}
