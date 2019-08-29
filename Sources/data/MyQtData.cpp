#include "MyQtData.h"

MyQtData::MyQtData() : selectedListItem_(nullptr) {}

std::list<HCMRData>* MyQtData::getDataList()
{
	return &dataList_;
}

HCMRData* MyQtData::getSelectedListItem()
{
	return selectedListItem_;
}

int MyQtData::getSelectedListItemIndex()
{
	return selectedListItemIndex_;
}

void MyQtData::setSelectedListItem(int index)
{
	int currentIndex = 0;
	for (HCMRData& data : this->dataList_)
	{
		if (currentIndex == index)
		{
			selectedListItem_ = &data;
			selectedListItemIndex_ = index;
			break;
		}
		currentIndex++;
	}
}

bool MyQtData::addDataToList(HCMRData newdata)
{
	bool exists = false;
	for (const HCMRData& data : dataList_)
	{
		if (data._fileName == newdata._fileName && data._filePath == newdata._filePath)
		{
			exists = true;
			break;
		}
	}

	if (!exists)
	{
		dataList_.push_back(newdata);
	}

	return !exists;
}

void  MyQtData::clearDataList()
{
	dataList_.clear();
	selectedListItemIndex_ = -1;
	selectedListItem_ = nullptr;
}
