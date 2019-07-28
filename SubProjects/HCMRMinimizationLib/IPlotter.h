#ifndef PLOTTER_INTERFACE_H
# define PLOTTER_INTERFACE_H
#include <vector>

class Plotter
{
public:
	virtual void plot(std::vector<double> vector, int startChannel, int graphNum) = 0;
	virtual void plot(std::vector<double> vector, std::vector<int> x, std::vector<int> y, int graphNum) = 0;
};

#endif //PLOTTER_INTERFACE_H
