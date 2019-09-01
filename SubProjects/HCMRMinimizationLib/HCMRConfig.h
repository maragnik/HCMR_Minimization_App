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
	std::vector<PeakSearchConfigEntry> _peakSearchConfigEntries;
};

#endif // HCMRCONFIG__H
