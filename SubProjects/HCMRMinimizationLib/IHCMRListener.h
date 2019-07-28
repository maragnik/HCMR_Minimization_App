#ifndef I_HCMR_LISTENER_H
#define I_HCMR_LISTENER_H
#include <vector>

class IHCMRListener
{
public:
	virtual void minuitChanged(std::vector<double> vector, int graphNum) = 0;
};

#endif //I_HCMR_LISTENER_H
