#ifndef FBSQL_IMPL_H_7832
#define FBSQL_IMPL_H_7832
#include "fbsql.h"
#include "ibpp/core/ibpp.h"
#include "error.h"

struct fbsql_database {
   IBPP::Database db;
   fbsql_database(
      const char *host, 
      const char *db, 
      const char *user, 
      const char *passwd, 
      const char *role,
      const char *charset,
      const char *create_params): db(IBPP::DatabaseFactory(host,db,user,passwd,role,charset,create_params)) {
   }
};

struct fbsql_transaction {
   IBPP::Transaction tr;
   fbsql_transaction(
      IBPP::Database db, 
      IBPP::TAM tam, 
      IBPP::TIL til, 
      IBPP::TLR tlr, 
      IBPP::TFF tff): 
   tr(IBPP::TransactionFactory(db,tam,til,tlr,tff)) { }
   fbsql_transaction(IBPP::Database db): tr(IBPP::TransactionFactory(db)) { }
};

struct fbsql_statement {
   IBPP::Statement st;
   fbsql_statement(
      IBPP::Database db,
      IBPP::Transaction tr): st(IBPP::StatementFactory(db,tr)) { }
};

struct fbsql_plan {
   char *plan;
};

struct fbsql_timestamp {
   IBPP::Timestamp ts;
};

struct fbsql_blob {
   IBPP::Blob b;
   fbsql_blob(IBPP::Database db, IBPP::Transaction tr): b(IBPP::BlobFactory(db,tr)){ }
   fbsql_blob(){ }
};

struct fbsql_dbkey {
   IBPP::DBKey k;
};

struct fbsql_users {
   unsigned count;
   char **users;
};

struct fbsql_service {
   IBPP::Service s;
   fbsql_service(const char *server, const char *user, const char *passwd): 
      s(IBPP::ServiceFactory(server,user,passwd)) { }
};

struct fbsql_user_list {
   std::vector<IBPP::User> users;
};

#endif
