#ifndef PLOT_SPECTRUM_H
#define PLOT_SPECTRUM_H
#include "HCMRSpectrum.h"
#include "qcustomplot.h"
#include <vector>
#include "IHCMRListener.h"

class MyQtListener : public IHCMRListener
{
public:
	MyQtListener();
	void minuitChanged(std::vector<double> vector, int graphNum) override;

};
#endif //PLOT_SPECTRUM_H
