#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "MyQtData.h"
#include "HCMRData.h"
#include "HCMRPeekFinder.h"
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
	void myplot();
	void dataFileAdded();

private slots:
	void browseForDataFile();
	void setDataYAxisScaleType(bool isChecked);
	void peekConfigChanged(int val);
	void peekConfigChanged(double val);
	void peekConfigChanged();
	void dataSelectionChanged(int selectedIndex);
	void removeSelectedDataFile();
	void removeAllDataFiles();
	void resetDataPlotScale();

private:
	Ui::MainWindow* ui;
	MyQTPlot dataPlot_;

	HCMRData _data;
	MyQtData myQtData_;
	HCMRPeekFinder peekFinder_;
};

#endif // MAINWINDOW_H
