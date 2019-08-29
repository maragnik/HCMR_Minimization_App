#ifndef HCMRCONFIG__H
#define HCMRCONFIG__H

#include <vector>

struct PeekSearchConfigEntry
{
	int centerChannel = -1;
	int numOfChannelsToTheLeft = 100;
	int numOfChannelsToTheRight = 100;
	bool searchRelativeToOtherPeek = false;
	int otherPeekConfigEntryIndex = -1;
	int numOfChannelsToTheLeftOfOtherPeek = 20;
	int numOfChannelsToTheRightOfOtherPeek = 20;
};

class HCMRConfig
{
public:
	HCMRConfig();
	std::vector<PeekSearchConfigEntry> _peekSearchConfigEntries;
};

#endif // HCMRCONFIG__H
