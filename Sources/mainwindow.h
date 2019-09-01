#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "MyQtData.h"
#include "HCMRData.h"
#include "HCMRPeakFinder.h"
#include "HCMRConfig.h"
#include "MyQTPlot.h"


namespace Ui {
	class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

public:
	explicit MainWindow(QWidget* parent = nullptr);
	~MainWindow();

	//============
	//  Data Tag  
	// ===========
public:
	void dataFileAdded();
	void refreshCalibrationPeakList();

private slots:
	void browseForDataFile();
	void setDataYAxisScaleType(bool isChecked);
	void peakConfigChanged(int val);
	void peakConfigChanged(double val);
	void peakConfigChanged();
	void dataSelectionChanged(int selectedIndex);
	void peakSelectionChanged(int peakIndex);
	void removeSelectedDataFile();
	void removeAllDataFiles();
	void resetDataPlotScale();
	void toolBoxPageChanged(int currentPageIndex);
	void openDataFilesToolsOpened();
	void choosePeaksToolsOpened();
	void peakSelected(QListWidgetItem* item);
	void addSelectedPeakForCalibration();
	void addSelectedPeakForCalibration(QListWidgetItem* item);
	void removeSelectedCalibrationPeak();
	void removeAllCalibrationPeaks();
	void peakCalibrationSelected(QListWidgetItem* item);


private:
	Ui::MainWindow* ui;
	MyQTPlot dataPlot_;


	MyQtData myQtData_;
	HCMRConfig config_;
	HCMRPeakFinder peakFinder_;
	int selectedCalibrationPeakIndex_;
};

#endif // MAINWINDOW_H
