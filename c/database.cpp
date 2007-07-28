#include "fbsql_impl.h"

extern "C" {

void fbsql_test() {
   printf("Hallo, Welt!\n");
}

fbsql_database *fbsql_database_new_default(const char *db) {
   FBSQL_TRY
   return fbsql_database_new("",db,"sysdba","5735","","","");
   FBSQL_CATCH(return 0)
}

fbsql_database *fbsql_database_new(
   const char *host, 
   const char *db, 
   const char *user, 
   const char *passwd, 
   const char *role,
   const char *charset,
   const char *create_params) {
   FBSQL_TRY
   return new fbsql_database(host, db, user, passwd, role, charset, create_params);
   FBSQL_CATCH(return 0)
}

void fbsql_database_connect(fbsql_database *db) {
   FBSQL_TRY
   db->db->Connect();
   FBSQL_CATCH(return)
}

int fbsql_database_connected(fbsql_database *db) {
   FBSQL_TRY
   return db->db->Connected();
   FBSQL_CATCH(return false)
}

void fbsql_database_inactivate(fbsql_database *db) {
   FBSQL_TRY
   db->db->Inactivate();
   FBSQL_CATCH(return)
}

void fbsql_database_disconnect(fbsql_database *db) {
   FBSQL_TRY
   db->db->Disconnect();
   FBSQL_CATCH(return)
}

void fbsql_database_create(fbsql_database *db, int dialect) {
   FBSQL_TRY
   db->db->Create(dialect);
   FBSQL_CATCH(return)
}

void fbsql_database_drop(fbsql_database *db) {
   FBSQL_TRY
   db->db->Drop();
   FBSQL_CATCH(return)
}

void fbsql_database_statistics(fbsql_database *db, int *fetches, int *marks, int *reads, int *writes) {
   FBSQL_TRY
   db->db->Statistics(fetches,marks,reads,writes);
   FBSQL_CATCH(return)
}

void fbsql_database_counts(fbsql_database *db, int *ins, int *upd, int *del, int *readidx, int *readseq) {
   FBSQL_TRY
   db->db->Counts(ins,upd,del,readidx,readseq);
   FBSQL_CATCH(return)
}

struct fbsql_users *fbsql_database_users(fbsql_database *db) {
   FBSQL_TRY
   std::vector<std::string> vusers;
   db->db->Users(vusers);
   if( vusers.size() > 0 ) {
      fbsql_users *result = new fbsql_users;
      result->count = vusers.size();
      result->users = reinterpret_cast<char**>(malloc(vusers.size()+1));
      for(size_t i = 0; i < vusers.size(); ++i) {
         (result->users)[i] = reinterpret_cast<char*>(malloc(vusers[i].size()+1));
         std::copy(vusers[i].begin(),vusers[i].end(),(result->users)[i]);
         (result->users)[i][vusers[i].size()] = '\0';
      }
      return result;
   }
   return 0;
   FBSQL_CATCH(return 0)
}

unsigned fbsql_users_count(struct fbsql_users *us) {
   FBSQL_TRY
   return us->count;
   FBSQL_CATCH(return 0)
}

const char *fbsql_users_get(struct fbsql_users *us, unsigned index) {
   FBSQL_TRY
   return us->users[index];
   FBSQL_CATCH(return 0)
}

void fbsql_users_delete(struct fbsql_users *us) {
   FBSQL_TRY
   delete us;
   FBSQL_CATCH(return)
}

int fbsql_database_dialect(fbsql_database *db) {
   FBSQL_TRY
   return db->db->Dialect();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_servername(fbsql_database *db) {
   FBSQL_TRY
   return db->db->ServerName();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_database(fbsql_database *db) {
   FBSQL_TRY
   return db->db->DatabaseName();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_user(fbsql_database *db) {
   FBSQL_TRY
   return db->db->Username();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_password(fbsql_database *db) {
   FBSQL_TRY
   return db->db->UserPassword();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_role(fbsql_database *db) {
   FBSQL_TRY
   return db->db->RoleName();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_charset(fbsql_database *db) {
   FBSQL_TRY
   return db->db->CharSet();
   FBSQL_CATCH(return 0)
}

const char *fbsql_database_create_params(fbsql_database *db) {
   FBSQL_TRY
   return db->db->CreateParams();
   FBSQL_CATCH(return 0)
}

void fbsql_database_delete(fbsql_database *db) {
   FBSQL_TRY
   delete db;
   FBSQL_CATCH(return)
}

}
