#include <iostream> // Hack to make sure we are using the correct length
                    // function
#include <cstring>  // Hack to make sure the correct memcpy is called
#include <string>
#include <boost/uuid.hpp>

#include <R.h>
#include "util.h"


using namespace boost;

extern "C"
{

SEXP boost_create_uuid()
{
  std::string newUuid = uuid::create().to_string();
  return String2RChar(newUuid);
}

} // extern "C"
