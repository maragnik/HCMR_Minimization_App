#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "HCMRParser.h"
#include "MyQTPlot.h"
#include "HCMRPeakFinder.h"
#include "loguru.h"
#include <iostream>
#include "common_definitions.h"
#include <list>


MainWindow::MainWindow(QWidget* parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	ui->setupUi(this);
	dataPlot_.setCustomPlot(ui->customPlotData);
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
	connect(ui->pushButton_add_peak_for_calibration, SIGNAL(clicked()), this, SLOT(addSelectedPeakForCalibration()));
	connect(ui->pushButton_remove_calibration_peak, SIGNAL(clicked()), this, SLOT(removeSelectedCalibrationPeak()));
	connect(ui->pushButton_remove_all_calibration_peaks, SIGNAL(clicked()), this, SLOT(removeAllCalibrationPeaks()));
	connect(ui->checkbox_yAxisScaleType, SIGNAL(toggled(bool)), this, SLOT(setDataYAxisScaleType(bool)));
	connect(ui->spinBox_minPeakRange, SIGNAL(valueChanged(int)), this, SLOT(peakConfigChanged(int)));
	connect(ui->spinBox_minPeakRangeNoEdge, SIGNAL(valueChanged(int)), this, SLOT(peakConfigChanged(int)));
	connect(ui->spinBox_minPeakToValey, SIGNAL(valueChanged(double)), this, SLOT(peakConfigChanged(double)));
	connect(ui->spinBox_minPeakWidth, SIGNAL(valueChanged(int)), this, SLOT(peakConfigChanged(int)));
	connect(ui->list_open_data_files, SIGNAL(currentRowChanged(int)), this, SLOT(dataSelectionChanged(int)));
	connect(ui->list_peaks, SIGNAL(currentRowChanged(int)), this, SLOT(peakSelectionChanged(int)));
	connect(ui->list_peaks, SIGNAL(itemClicked(QListWidgetItem*)), this, SLOT(peakSelected(QListWidgetItem*)));
	connect(ui->list_peaks, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(addSelectedPeakForCalibration(QListWidgetItem*)));
	connect(ui->list_calibration_peaks, SIGNAL(itemClicked(QListWidgetItem*)), this, SLOT(peakCalibrationSelected(QListWidgetItem*)));
	connect(ui->pushButton_resetScale, SIGNAL(clicked()), this, SLOT(resetDataPlotScale()));
	connect(ui->customPlotData->xAxis, SIGNAL(rangeChanged(QCPRange)), ui->customPlotData->xAxis2, SLOT(setRange(QCPRange)));
	connect(ui->toolBox, SIGNAL(currentChanged(int)), this, SLOT(toolBoxPageChanged(int)));
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
	peakFinder_.findAllPeaks(myQtData_.getSelectedListItem()->getSpectum().getDataVector());
	dataPlot_.plotRowData(selectedData->_spectrum.getDataVector(), 0);
	peakConfigChanged();
}

void MainWindow::peakSelectionChanged(int peakIndex)
{
	if (peakIndex >= 0)
	{
		dataPlot_.plotSelectedPeak(peakFinder_.finalPeaks_[peakIndex]);
	}
	else
	{
		HCMRPeak dummyPeak;
		dummyPeak.isValid = false;
		dataPlot_.plotSelectedPeak(dummyPeak);
	}
}

void MainWindow::resetDataPlotScale()
{
	dataPlot_.resetInitialPlotRange();
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
		dataPlot_.getCustomPlot()->yAxis->setScaleType(QCPAxis::stLinear);
		QSharedPointer<QCPAxisTicker> ticker(new QCPAxisTicker);
		dataPlot_.getCustomPlot()->yAxis->setTicker(ticker);
	}

	else
	{
		dataPlot_.getCustomPlot()->yAxis->setScaleType(QCPAxis::stLogarithmic);
		QSharedPointer<QCPAxisTickerLog> logTicker(new QCPAxisTickerLog);
		dataPlot_.getCustomPlot()->yAxis->setTicker(logTicker);
	}


	dataPlot_.getCustomPlot()->replot();

}

void MainWindow::peakConfigChanged(int val)
{
	peakConfigChanged();
}
void MainWindow::peakConfigChanged(double val)
{
	peakConfigChanged();
}
void MainWindow::peakConfigChanged()
{
	int minPeakRange = ui->spinBox_minPeakRange->value();
	int minPeakRangeNoEdge = ui->spinBox_minPeakRangeNoEdge->value();
	double minPeakToValey = static_cast<double>(ui->spinBox_minPeakToValey->value());
	int minPeakWidth = ui->spinBox_minPeakWidth->value();

	peakFinder_.choosePeaks(minPeakRange, minPeakRangeNoEdge, minPeakToValey, minPeakWidth);
	dataPlot_.plotPeaks(peakFinder_.finalPeaks_, 1);
	ui->list_peaks->clear();
	for (int i = 0; i < peakFinder_.finalPeaks_.size(); ++i)
	{
		QString str;
		str.sprintf("%.2d. channel: %.4d, PtV: %5.2f, peakWidth: %d, width: %d, widthNoEdge: %d",
			i,
			peakFinder_.finalPeaks_[i].channel,
			peakFinder_.finalPeaks_[i].peakToValey * 100 / peakFinder_.finalPeaks_[i].value,
			peakFinder_.finalPeaks_[i].widthPtV,
			peakFinder_.finalPeaks_[i].width,
			peakFinder_.finalPeaks_[i].widthNoEdge);
		ui->list_peaks->addItem(str);
	}
}

void MainWindow::toolBoxPageChanged(int currentPageIndex)
{
	if (currentPageIndex == 0)
	{
		openDataFilesToolsOpened();
	}
	else if (currentPageIndex == 1)
	{
		choosePeaksToolsOpened();
	}
}

void MainWindow::openDataFilesToolsOpened()
{

}

void MainWindow::choosePeaksToolsOpened()
{
	peakFinder_.findAllPeaks(myQtData_.getSelectedListItem()->getSpectum().getDataVector());
	peakConfigChanged();
}

void MainWindow::peakSelected(QListWidgetItem * item)
{
	int selectedPeakIndex = ui->list_peaks->row(item);
	dataPlot_.plotSelectedPeak(peakFinder_.finalPeaks_[selectedPeakIndex], 2);
}

void MainWindow::addSelectedPeakForCalibration()
{
	int index = ui->list_peaks->currentRow();
	PeakSearchConfigEntry peakSearchConfigEntry;
	peakSearchConfigEntry.centerChannel = peakFinder_.finalPeaks_[index].channel;
	config_.addPeakSearchConfigEntry(peakSearchConfigEntry);
	refreshCalibrationPeakList();
}

void MainWindow::addSelectedPeakForCalibration(QListWidgetItem * item)
{
	addSelectedPeakForCalibration();
}

void MainWindow::refreshCalibrationPeakList()
{
	ui->list_calibration_peaks->clear();
	if (!config_.peakSearchConfigEntries.empty())
	{
		ui->toolBox->setItemEnabled(2, true);
	}
	else
	{
		ui->toolBox->setItemEnabled(2, false);
	}

	for (int i = 0; i < config_.peakSearchConfigEntries.size(); ++i)
	{
		QString str;
		str.sprintf("%d. Channel: %.4d", i, config_.peakSearchConfigEntries[i].centerChannel);
		ui->list_calibration_peaks->addItem(str);
	}
	dataPlot_.plotCalibrationPeaks(config_.peakSearchConfigEntries);
}

void MainWindow::removeSelectedCalibrationPeak()
{
	int index = ui->list_calibration_peaks->currentRow();
	if (index >= 0)
	{
		config_.removePeakSearchConfigEntry(index);
		refreshCalibrationPeakList();
		dataPlot_.plotCalibrationPeaks(config_.peakSearchConfigEntries);
	}

}
void MainWindow::removeAllCalibrationPeaks()
{
	config_.peakSearchConfigEntries.clear();
	refreshCalibrationPeakList();
	dataPlot_.plotCalibrationPeaks(config_.peakSearchConfigEntries);
}

void MainWindow::peakCalibrationSelected(QListWidgetItem * item)
{

	int index = ui->list_calibration_peaks->row(item);
	dataPlot_.highlightSelectedCalibrationPeak(index);
}
