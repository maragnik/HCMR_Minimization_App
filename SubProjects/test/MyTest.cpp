#include "HCMRMinimization.h"
#include "Minuit2/FCNBase.h"
#include "Minuit2/MnMigrad.h"
#include "Minuit2/FunctionMinimum.h"
#include "common_definitions.h"
#include <string>
#include "loguru.h"
#include "HCMRParser.h"
#include "HCMRPeekFinder.h"


void testMinuit2()
{
	LOG_F(INFO, "Test Minimization with MINUT2");
	HCMRMinimization minimization;
	minimization.addData(PATH_TO_EXTERNALS + "/testRandom.txt");
	minimization.addUnitaryToModel(PATH_TO_EXTERNALS + "/testModel.txt");
	minimization._data.print();

	// create Minuit parameters with names
	ROOT::Minuit2::MnUserParameters upar;
	upar.Add("b", 10., 0.1);

	// create MIGRAD minimizer
	ROOT::Minuit2::MnMigrad migrad(minimization, upar);
	ROOT::Minuit2::FunctionMinimum min = migrad();
}

void testLogger()
{
	loguru::g_stderr_verbosity = 5;
	LOG_F(0, "Test Logger verbosity 0 (verbosity set to 5)");
	LOG_F(1, "Test Logger verbosity 1 (verbosity set to 5)");
	LOG_F(2, "Test Logger verbosity 2 (verbosity set to 5)");
	LOG_F(3, "Test Logger verbosity 3 (verbosity set to 5)");
	LOG_F(4, "Test Logger verbosity 4 (verbosity set to 5)");
	LOG_F(5, "Test Logger verbosity 5 (verbosity set to 5)");
	LOG_F(6, "Test Logger verbosity 6 (verbosity set to 5)");
	LOG_F(7, "Test Logger verbosity 7 (verbosity set to 5)");
	LOG_F(INFO, "I'm hungry for some %.3f!", 3.14159);
	LOG_F(WARNING, "I'm hungry for some %.3f!", 3.14159);
	LOG_F(ERROR, "I'm hungry for some %.3f!", 3.14159);
	//LOG_F(FATAL, "I'm hungry for some %.3f!", 3.14159);

	LOG_IF_F(ERROR, true, "Will only show if true");
	LOG_IF_F(ERROR, false, "Will not show if false");
}

void testExpData()
{
}

int main(int argc, char* argv[]) {

	loguru::init(argc, argv);
	loguru::add_file("test.log", loguru::Append, loguru::Verbosity_MAX);
	testExpData();
	return 0;
}
