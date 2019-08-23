#ifndef MYQTDATA_H
#define MYQTDATA_H

#include <list>
#include "HCMRData.h"

class MyQtData
{
public:
	MyQtData();
	std::list<HCMRData>* getDataList();
	HCMRData* getSelectedListItem();
	void setSelectedListItem(int index);
	bool addDataToList(HCMRData newdata);
	void clearDataList();

private:
	std::list<HCMRData> dataList_;
	HCMRData* selectedListItem_;
	long int selectedListItemIndex_;

};
#endif //MYQTDATA_H
