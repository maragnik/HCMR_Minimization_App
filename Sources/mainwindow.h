#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "MyQtData.h"
#include "HCMRData.h"
#include "HCMRPeakFinder.h"
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

private slots:
	void browseForDataFile();
	void setDataYAxisScaleType(bool isChecked);
	void peakConfigChanged(int val);
	void peakConfigChanged(double val);
	void peakConfigChanged();
	void dataSelectionChanged(int selectedIndex);
	void removeSelectedDataFile();
	void removeAllDataFiles();
	void resetDataPlotScale();
	void toolBoxPageChanged(int currentPageIndex);
	void openDataFilesToolsOpened();
	void choosePeaksToolsOpened();
	void peakHovered(QListWidgetItem* item);

private:
	Ui::MainWindow* ui;
	MyQTPlot dataPlot_;

	HCMRData _data;
	MyQtData myQtData_;
	HCMRPeakFinder peakFinder_;
};

#endif // MAINWINDOW_H
