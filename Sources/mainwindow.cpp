#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "HCMRParser.h"
#include "QTPlotter.h"
#include "HCMRPeekFinder.h"
#include "loguru.h"
#include <iostream>
#include "common_definitions.h"
#include <list>


MainWindow::MainWindow(QWidget* parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	ui->setupUi(this);
	ui->frame_plot->hide();
	ui->groupBox_file_info->hide();
	ui->toolBox->setItemEnabled(1, false);
	ui->toolBox->setItemEnabled(2, false);
	ui->customPlotData->setInteraction(QCP::iRangeDrag, true);
	ui->customPlotData->setInteraction(QCP::iRangeZoom, true);
	//ui->customPlotData->xAxis2->setVisible(true);
	setDataYAxisScaleType(ui->checkbox_yAxisScaleType->isChecked());

	connect(ui->button_add_data_file, &QAbstractButton::clicked, this, &MainWindow::browseForDataFile);
	connect(ui->button_remove_all_data_files, SIGNAL(clicked()), this, SLOT(removeAllDataFiles()));
	connect(ui->button_remove_selected_data_file, SIGNAL(clicked()), this, SLOT(removeSelectedDataFile()));
	connect(ui->checkbox_yAxisScaleType, SIGNAL(toggled(bool)), this, SLOT(setDataYAxisScaleType(bool)));
	connect(ui->spinBox_minPeekRange, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->spinBox_minPeekRangeNoEdge, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->spinBox_minPeekToValey, SIGNAL(valueChanged(double)), this, SLOT(peekConfigChanged(double)));
	connect(ui->spinBox_minPeekWidth, SIGNAL(valueChanged(int)), this, SLOT(peekConfigChanged(int)));
	connect(ui->list_open_data_files, SIGNAL(currentRowChanged(int)), this, SLOT(dataSelectionChanged(int)));

	connect(ui->customPlotData->xAxis, SIGNAL(rangeChanged(QCPRange)), ui->customPlotData->xAxis2, SLOT(setRange(QCPRange)));
}

MainWindow::~MainWindow()
{
	delete ui;
}

void MainWindow::dataSelectionChanged(int selectedIndex)
{
	myQtData_.setSelectedListItem(selectedIndex);
	HCMRData* selectedData = myQtData_.getSelectedListItem();

	// Fill the data info fields
	ui->lineEdit_data_file_name->setText(QString::fromStdString(selectedData->_fileName));
	ui->lineEdit_data_file_path->setText(QString::fromStdString(selectedData->_filePath));
	ui->lineEdit_data_numOfChannels->setText(QString::number(selectedData->_numOfChanels));
	ui->dateEdit_data_collected->setDate(QDate::fromString(QString::fromStdString(selectedData->_dateCollected), "dd/MM/yyyy"));
	ui->timeEdit_data_collected->setTime(QTime::fromString(QString::fromStdString(selectedData->_timeCollected), "HH:mm:ss"));
	ui->lineEdit_MeasDurationLive->setText(QString::number(selectedData->_liveMeassurementDuration));
	ui->lineEdit_MeasDurationReal->setText(QString::number(selectedData->_realMeassurementDuration));
	if (selectedData->_keVperChannel != 1 || selectedData->_offsetInKeV != 0)
	{
		ui->lineEdit_data_calibration_slop->setText(QString::number(selectedData->_keVperChannel));
		ui->lineEdit_data_calibration_offset->setText(QString::number(selectedData->_offsetInKeV));
	}
	else
	{
		ui->lineEdit_data_calibration_slop->setText("Not Calibrated");
		ui->lineEdit_data_calibration_offset->setText("Not Calibrated");
	}

	// Fill the graph
	plotter_.setGraph(ui->customPlotData);
	plotter_.plotRowData(selectedData->_spectrum.getDataVector(), 0);


}

void MainWindow::dataFileAdded()
{
	// Hold selected row
	int selectedRow = 0;
	if (ui->list_open_data_files->count() > 0)
	{
		selectedRow = ui->list_open_data_files->currentRow();
	}

	// Refresh list
	ui->list_open_data_files->clear();
	for (HCMRData data : *(myQtData_.getDataList()))
	{
		QString fileName = QString::fromStdString(data.getSpectum().getName());
		ui->list_open_data_files->addItem(fileName);
	}

	// Set the previously sellected item
	ui->list_open_data_files->item(selectedRow)->setSelected(true);
	ui->list_open_data_files->setCurrentRow(selectedRow);


	// Show plot and info
	if (ui->list_open_data_files->count() == 1)
	{
		ui->frame_plot->show();
		ui->groupBox_file_info->show();
	}

	// Enable ui objects
	ui->toolBox->setItemEnabled(1, true);
}

void MainWindow::removeSelectedDataFile()
{
	int selectedIndex = ui->list_open_data_files->currentRow();
	if (myQtData_.getDataList()->size() <= 1)
	{
		removeAllDataFiles();
	}
	else
	{
		if (selectedIndex < myQtData_.getDataList()->size() - 1)
		{
			ui->list_open_data_files->item(selectedIndex + 1)->setSelected(true);
			ui->list_open_data_files->setCurrentRow(selectedIndex + 1);
		}
		else
		{
			ui->list_open_data_files->item(selectedIndex - 1)->setSelected(true);
			ui->list_open_data_files->setCurrentRow(selectedIndex - 1);
		}
		std::list<HCMRData>::iterator it;
		it = myQtData_.getDataList()->begin();
		advance(it, selectedIndex);
		myQtData_.getDataList()->erase(it);
		ui->list_open_data_files->takeItem(selectedIndex);
	}
}

void MainWindow::removeAllDataFiles()
{
	ui->toolBox->setItemEnabled(1, false);
	ui->list_open_data_files->clear();
	ui->frame_plot->hide();
	ui->groupBox_file_info->hide();
	myQtData_.clearDataList();

}

void MainWindow::myplot()
{
	plotter_.setGraph(ui->customPlotData);



	plotter_.plotRowData(_data.getSpectum().getDataVector(), 0);

	peekFinder_.setData(_data.getSpectum().getDataVector());
	peekFinder_.findAllPeaks(_data.getSpectum().getDataVector());
	peekConfigChanged();
}

void MainWindow::browseForDataFile()
{
	QString path = QString::fromStdString(PATH_TO_EXTERNALS);
	if (!myQtData_.getDataList()->empty())
	{
		path = QString::fromStdString(myQtData_.getDataList()->back()._filePath);
	}

	QFileDialog dialog(this);
	dialog.setDirectory(path);
	dialog.setFileMode(QFileDialog::ExistingFiles);
	dialog.setViewMode(QFileDialog::Detail);
	dialog.setNameFilter("Spectra (*.spe)");
	dialog.setWindowTitle("Open Spectrum File");

	QStringList selectedFiles;
	if (dialog.exec())
	{
		selectedFiles = dialog.selectedFiles();
		if (selectedFiles.size() > 0)
		{
			for (QString fileName : selectedFiles)
			{
				HCMRData data;
				data.fillDataFromSpeFile(fileName.toStdString());
				if (myQtData_.addDataToList(data))
				{
					dataFileAdded();
					//myplot();
				}
			}
		}
	}
}

void MainWindow::setDataYAxisScaleType(bool isChecked)
{
	if (!isChecked)
	{
		ui->customPlotData->yAxis->setScaleType(QCPAxis::stLinear);
		QSharedPointer<QCPAxisTicker> ticker(new QCPAxisTicker);
		ui->customPlotData->yAxis->setTicker(ticker);
	}

	else
	{
		ui->customPlotData->yAxis->setScaleType(QCPAxis::stLogarithmic);
		QSharedPointer<QCPAxisTickerLog> logTicker(new QCPAxisTickerLog);
		ui->customPlotData->yAxis->setTicker(logTicker);
	}


	ui->customPlotData->replot();

}

void MainWindow::peekConfigChanged(int val)
{
	peekConfigChanged();
}
void MainWindow::peekConfigChanged(double val)
{
	peekConfigChanged();
}
void MainWindow::peekConfigChanged()
{
	int minPeekRange = ui->spinBox_minPeekRange->value();
	int minPeekRangeNoEdge = ui->spinBox_minPeekRangeNoEdge->value();
	double minPeekToValey = static_cast<double>(ui->spinBox_minPeekToValey->value());
	int minPeekWidth = ui->spinBox_minPeekWidth->value();

	peekFinder_.choosePeaks(minPeekRange, minPeekRangeNoEdge, minPeekToValey, minPeekWidth);
	ui->customPlotData->addGraph();
	plotter_.setUpForPeekPlot();
	plotter_.plotPeeks(peekFinder_.finalPeeks_);
	//plotter_.plotFullPeeks(_data.getSpectum().getDataVector(), peekFinder_.finalPeeks_);
	ui->list_peeks->clear();
	for (int i = 0; i < peekFinder_.finalPeeks_.size(); ++i)
	{
		ui->list_peeks->addItem(QString::number(peekFinder_.finalPeeks_[i].channel) + "  " +
			QString::number(peekFinder_.finalPeeks_[i].widthNoEdge) + "  " +
			QString::number(peekFinder_.finalPeeks_[i].width) + "  " +
			QString::number(peekFinder_.finalPeeks_[i].widthPtV) + "  " +
			QString::number(peekFinder_.finalPeeks_[i].peekToValey));
	}
}
