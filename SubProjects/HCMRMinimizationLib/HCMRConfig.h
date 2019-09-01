#ifndef HCMRCONFIG__H
#define HCMRCONFIG__H

#include <vector>

struct PeakSearchConfigEntry
{
	int centerChannel = -1;
	int numOfChannelsToTheLeft = 100;
	int numOfChannelsToTheRight = 100;
	bool searchRelativeToOtherPeak = false;
	int otherPeakConfigEntryIndex = -1;
	int numOfChannelsToTheLeftOfOtherPeak = 20;
	int numOfChannelsToTheRightOfOtherPeak = 20;
};

class HCMRConfig
{
public:
	HCMRConfig();
	void addPeakSearchConfigEntry(PeakSearchConfigEntry peakSearchConfigEntry);
	void removePeakSearchConfigEntry(int index);
	std::vector<PeakSearchConfigEntry> peakSearchConfigEntries;
};

#endif // HCMRCONFIG__H
