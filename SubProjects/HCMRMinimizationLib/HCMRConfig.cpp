#include "HCMRConfig.h"

HCMRConfig::HCMRConfig()
{
	peakSearchConfigEntries.clear();
}

void HCMRConfig::addPeakSearchConfigEntry(PeakSearchConfigEntry peakSearchConfigEntry)
{
	for (const PeakSearchConfigEntry& entry : peakSearchConfigEntries)
	{
		if (entry.centerChannel == peakSearchConfigEntry.centerChannel)
		{
			return;
		}
	}
	peakSearchConfigEntries.push_back(peakSearchConfigEntry);
}

void HCMRConfig::removePeakSearchConfigEntry(int index)
{
	std::vector<PeakSearchConfigEntry>::iterator it = peakSearchConfigEntries.begin() + index;
	peakSearchConfigEntries.erase(it);
}
