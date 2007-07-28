#include <fbsql.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
   struct fbsql_database    *db;
   struct fbsql_transaction *tr;
   struct fbsql_statement   *st;
   int id;
   db = fbsql_database_new("localhost","/pub/firebird/Test1.fdb","sysdba","5735","","iso8859_1","");
   fbsql_database_connect(db);

   tr = fbsql_transaction_new_with_defaults(db);
   fbsql_transaction_start(tr);

   st = fbsql_statement_new(db,tr);
   fbsql_statement_prepare(st,"select id from test");
   fbsql_statement_execute(st);
   while(fbsql_statement_fetch(st)) {
      if( fbsql_statement_get_integer(st, 1, &id) )
         printf("ID: NULL\n");
      else
         printf("ID: %d\n",id);
   }
   fbsql_statement_close(st);
   fbsql_statement_delete(st);
   fbsql_transaction_rollback(tr);
   fbsql_transaction_delete(tr);
   fbsql_database_disconnect(db);
   fbsql_database_delete(db);
   return 0;
}

