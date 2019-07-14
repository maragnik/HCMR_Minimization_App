#include <QApplication>
#include "mainwindow.h"
#include "HCMRUnitaryInput.h"

int main(int argc, char* argv[]) {
	QApplication a(argc, argv);
	HCMRUnitaryInput<int, int> input;
	MainWindow w;
	w.show();
	return a.exec();
}
