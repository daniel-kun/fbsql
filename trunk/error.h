#ifndef ERROR_H_8923
#define ERROR_H_8923
#include <string>

void fbsql_error_set(const std::string &error);
#define FBSQL_TRY try {
#define FBSQL_CATCH(a) } catch(std::exception &e) { fbsql_error_set(e.what()); a; }

#endif
