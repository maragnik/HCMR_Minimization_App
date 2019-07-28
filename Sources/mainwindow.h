#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "HCMRData.h"
#include "HCMRPeekFinder.h"
#include "QTPlotter.h"


namespace Ui {
	class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

public:
	explicit MainWindow(QWidget* parent = nullptr);
	~MainWindow();

	void myplot();

private slots:
	void browseForDataFile();
	void setDataYAxisScaleType(bool isChecked);
	void peekConfigChanged(int val);

private:
	Ui::MainWindow* ui;
	HCMRData _data;
	HCMRPeekFinder _peekFinder;
	QTPlotter _plotter;
};

#endif // MAINWINDOW_H
