#include "../HCMRMinimizationLib/HCMRUnitaryInput.h"

int main(int argc, char* argv[]) {
	HCMRUnitaryInput<int, int> input;
	input.fillFromFile("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\test.csv");
	return 0;
}
