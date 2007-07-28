#include "fbsql.h"
#include <stdio.h>
#include <stdlib.h>

void test_print_users(struct fbsql_service *s) {
   struct fbsql_user_list *ul = fbsql_service_get_user_list(s);
   struct fbsql_user *u;
   unsigned i;
   unsigned bufsize;
   char *buf;
   for(i = 0; i < fbsql_user_list_count(ul); ++i) {
      u = fbsql_user_list_get(ul,i);
      bufsize = fbsql_user_get_name_size(u);
      buf = (char*)malloc(bufsize+1);
      fbsql_user_get_name(u,buf);
      printf("User: %s\n",buf);
      free(buf);
   }
   fbsql_user_list_delete(ul);
}

int main() {
   char dbg_buffer[1024];
   struct fbsql_statement *st;
   struct fbsql_database *db;
   struct fbsql_transaction *tr;
   struct fbsql_users *users;
   struct fbsql_service *s;
   struct fbsql_user *u;
   char *buffer;
   unsigned i;
/*   db = fbsql_database_new("localhost","C:\\Users\\Daniel Albuschat\\Database\\Test1.fdb","sysdba","masterkey","","","");
   fbsql_database_create(db);
   printf("%s\n",fbsql_database_user(db));
   users = fbsql_database_users(db);
   for(i = 0; i < fbsql_users_count(users); ++i)
      printf("User: %s\n",fbsql_users_get(users,i));
   fbsql_users_delete(users);

   tr = fbsql_transaction_new_with_defaults(db);
   fbsql_transaction_start(tr);

   st = fbsql_statement_new(db,tr);
   fbsql_statement_prepare(st,"select id, text from test");
   fbsql_statement_execute(st);

   while(fbsql_statement_fetch(st)) {
      int id, size;
      if( fbsql_statement_get_integer(st,1,&id) )
         id = 0;
      size = fbsql_statement_get_string_size(st,2);
      if( size > 0 ) {
         buffer = (char*)malloc(size);
         fbsql_statement_get_string(st,2,buffer);
         printf("\"%s\"\n",buffer);
      }
      printf("%d\n",id);
   }

   fbsql_transaction_commit_retaining(tr);
   fbsql_database_disconnect(db);*/
   s = fbsql_service_new("localhost","sysdba","5735");
   fbsql_service_connect(s);
//   test_print_users(s);
   u = fbsql_user_new();
   fbsql_user_set_name(u,"DANIEL2");
   fbsql_user_set_password(u,"keines");
   fbsql_service_add_user(s,u);
   test_print_users(s);
   fbsql_service_delete(s);
   scanf("%s",dbg_buffer);
   return 0;
}
