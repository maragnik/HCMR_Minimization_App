#ifndef HCMRCONFIG__H
#define HCMRCONFIG__H

#include <vector>
#include <string>

enum class PeakChooseMethod
{
	max_peak_to_valey = 0,
	max_peak_width = 1,
	max_peak_dominant_area = 2,
	max_peak_dominant_area_no_edge = 3
};

struct PeakSearchConfigEntry
{
	int id = -1;
	std::string description = "";
	double peakEnergy = 0;
	int centerChannel = -1;
	int numOfChannelsToTheLeft = 30;
	int numOfChannelsToTheRight = 30;
	bool searchRelativeToOtherPeak = false;
	int otherPeakConfigEntryId = -1;
	int channelDistanceFromReferencePeak = 0;
	int numOfChannelsToTheLeft2 = 10;
	int numOfChannelsToTheRight2 = 10;
	int minPeakWindow;
	int minPeakWindowNoEdges;
	double minPeakToValey;
	int minPeakWidth;
	PeakChooseMethod peakChooseMethod = PeakChooseMethod::max_peak_to_valey;
};

class HCMRConfig
{
public:
	HCMRConfig();
	void addPeakSearchConfigEntry(PeakSearchConfigEntry peakSearchConfigEntry);
	void removePeakSearchConfigEntry(int index);
	std::vector<PeakSearchConfigEntry> peakSearchConfigEntries;
private:
	static int nextId;
};

#endif // HCMRCONFIG__H
