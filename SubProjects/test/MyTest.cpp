#include "HCMRMinimization.h"
#include "Minuit2/FCNBase.h"
#include "Minuit2/MnMigrad.h"
#include "Minuit2/FunctionMinimum.h"

#include <cassert>

void testFileParsing()
{
	HCMRSpectrum input;
	input.fillFromFile("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testRandom.txt");
	input.print();
}

void test1()
{
	HCMRData data;
	data.fillFromFile("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testRandom.txt");
	HCMRModel model;
	model.addNewUnitary("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testModel.txt");
	model.addNewUnitary("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testModel.txt");
	model.addNewUnitary("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testModel.txt");
	model.print();
}

void test2()
{
	HCMRMinimization minimization;
	minimization.addData("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testRandom.txt");
	minimization.addUnitaryToModel("C:\\prj\\elkethe\\HCMR_Minimization_App\\Extra\\testModel.txt");
	std::cout << "Print Data -->" << std::endl;
	minimization._data.print();
	std::cout << "Print Model -->" << std::endl;
	minimization._model.print();

	// create Minuit parameters with names
	ROOT::Minuit2::MnUserParameters upar;
	upar.Add("b", 10., 0.1);

	// create MIGRAD minimizer
	ROOT::Minuit2::MnMigrad migrad(minimization, upar);
	ROOT::Minuit2::FunctionMinimum min = migrad();
}

double model(double x, double parameter)
{
	return parameter;
}

int main(int argc, char* argv[]) {

	//testFileParsing();
	//test1();
	test2();
	assert(true);
	return 0;
}
