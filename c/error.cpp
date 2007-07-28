#include "fbsql_impl.h"
#include <string>
#include <algorithm>

static std::string last_error;

int fbsql_error_occured() {
   return !last_error.empty();
}

unsigned fbsql_error_msg_size() {
   return last_error.size();
}

void fbsql_error_msg(char *ptr) {
   std::copy(last_error.begin(),last_error.end(),ptr);
   ptr[last_error.size()] = '\0';
}

void fbsql_error_set(const std::string &error) {
   last_error = error;
}
