#ifndef COMMON_DEFINITIONS_H
#define COMMON_DEFINITIONS_H

#include <string>
#include <map>

#ifdef PATH_TO_EXTERNALS_STRING 
const std::string PATH_TO_EXTERNALS = PATH_TO_EXTERNALS_STRING;
#else
#error PATH_TO_EXTRAS_STRING is not defined
#endif

using XType = double;
using YType = double;
using XYMapType = std::map<XType, YType>;
using PointType = std::pair<XType, YType>;
const double LOW_VALUE = std::numeric_limits<float>::min();

#endif //COMMON_DEFINITIONS_H
