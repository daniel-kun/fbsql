# FBSQL Project Goals #
fbsql aims at easing the use of Firebird and Interbase databases by providing clean client-libraries for different languages. The raw Firebird/Interbase-API is very awkward and hard to work with, while fbsql is easy and straight-forward like for example the sqlite- and mysql-APIs are. Currently **C** and **Lisp** are supported.
fbsql uses [IBPP](http://www.ibpp.org), a C++ Interbase/Firebird API for it's implementation. It's basically just a thin wrapper around IBPP to make it accessible in C, and therefore in any other language.
## Supported Platforms ##
Current Linux with GCC, Windows with mingw and Windows with Microsoft Visual Studio 2005 are tested. The code is rather portable, so I believe on every platform where the firebird client-library and IBPP are accessible, fbsql will work, too.
As for the Lisp-library, I've only tested it with CLISP on Linux, yet. Again, this should be rather portable to most Lisp-implementations and platforms, too.
## Example Code ##
Here's an example C-code which connects to a database and selects the IDs of the table "test":
```
#include "fbsql.h"
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
```
## Download ##
I've added a "Sneak-Preview" download today, that's the base of what will be the first release. It should already be pretty usable, basic tests all worked out.
Check it out at the [download-page](http://code.google.com/p/fbsql/downloads/list)!