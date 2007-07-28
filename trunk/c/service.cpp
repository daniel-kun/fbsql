#include "fbsql_impl.h"

struct fbsql_service *fbsql_service_new(
      const char *server,
      const char *user,
      const char *password) {
   FBSQL_TRY
   return new fbsql_service(server,user,password);
   FBSQL_CATCH(return 0)
}

void fbsql_service_connect            (struct fbsql_service *s) {
   FBSQL_TRY
   s->s->Connect();
   FBSQL_CATCH(return)
}

int  fbsql_service_connected          (struct fbsql_service *s) {
   FBSQL_TRY
   return s->s->Connected();
   FBSQL_CATCH(return 0)
}

void fbsql_service_disconnect         (struct fbsql_service *s) {
   FBSQL_TRY
   s->s->Disconnect();
   FBSQL_CATCH(return)
}

int  fbsql_service_get_version_size   (struct fbsql_service *s) {
   FBSQL_TRY
   std::string str;
   s->s->GetVersion(str);
   return str.size();
   FBSQL_CATCH(return 0)
}

void fbsql_service_get_version        (struct fbsql_service *s, char *buf) {
   FBSQL_TRY
   std::string str;
   s->s->GetVersion(str);
   std::copy(str.begin(),str.end(),buf);
   buf[str.size()] = '\0';
   FBSQL_CATCH(return)
}

void fbsql_service_add_user           (struct fbsql_service *s, struct fbsql_user *u) {
   FBSQL_TRY
   s->s->AddUser(*reinterpret_cast<IBPP::User*>(u));
   FBSQL_CATCH(return)
}

void fbsql_service_get_user           (struct fbsql_service *s, struct fbsql_user *u) {
   FBSQL_TRY
   s->s->GetUser(*reinterpret_cast<IBPP::User*>(u));
   FBSQL_CATCH(return)
}

void fbsql_service_modify_user        (struct fbsql_service *s, struct fbsql_user *u) {
   FBSQL_TRY
   s->s->ModifyUser(*reinterpret_cast<IBPP::User*>(u));
   FBSQL_CATCH(return)
}

void fbsql_service_remove_user        (struct fbsql_service *s, struct fbsql_user *u) {
   FBSQL_TRY
   s->s->ModifyUser(*reinterpret_cast<IBPP::User*>(u));
   FBSQL_CATCH(return)
}

void fbsql_service_set_page_buffers   (struct fbsql_service *s, const char *db, int buffers) {
   FBSQL_TRY
   s->s->SetPageBuffers(db,buffers);
   FBSQL_CATCH(return)
}

void fbsql_service_set_sweep_interval (struct fbsql_service *s, const char *db, int interval) {
   FBSQL_TRY
   s->s->SetSweepInterval(db,interval);
   FBSQL_CATCH(return)
}

void fbsql_service_set_sync_write     (struct fbsql_service *s, const char *db, int sync) {
   FBSQL_TRY
   s->s->SetSyncWrite(db,sync);
   FBSQL_CATCH(return)
}

void fbsql_service_set_read_only      (struct fbsql_service *s, const char *db, int readonly) {
   FBSQL_TRY
   s->s->SetReadOnly(db,readonly);
   FBSQL_CATCH(return)
}

void fbsql_service_set_reserve_space  (struct fbsql_service *s, const char *db, int reserve) {
   FBSQL_TRY
   s->s->SetReserveSpace(db,reserve);
   FBSQL_CATCH(return)
}

void fbsql_service_shutdown           (struct fbsql_service *s, const char *db, enum fbsql_DSM mode, int timeout) {
   FBSQL_TRY
   s->s->Shutdown(db,static_cast<IBPP::DSM>(mode),timeout);
   FBSQL_CATCH(return)
}

void fbsql_service_restart            (struct fbsql_service *s, const char *db) {
   FBSQL_TRY
   s->s->Restart(db);
   FBSQL_CATCH(return)
}

void fbsql_service_sweep              (struct fbsql_service *s, const char *db) {
   FBSQL_TRY
   s->s->Sweep(db);
   FBSQL_CATCH(return)
}

void fbsql_service_repair             (struct fbsql_service *s, const char *db, enum fbsql_RPF flags) {
   FBSQL_TRY
   s->s->Repair(db,static_cast<IBPP::RPF>(flags));
   FBSQL_CATCH(return)
}

void fbsql_service_start_backup       (struct fbsql_service *s, const char *db, const char *fbk, enum fbsql_BRF flags) {
   FBSQL_TRY
   s->s->StartBackup(db,fbk,static_cast<IBPP::BRF>(flags));
   FBSQL_CATCH(return)
}

void fbsql_service_start_restore      (struct fbsql_service *s, const char *fbk, const char *db, int pagesize, enum fbsql_BRF flags) {
   FBSQL_TRY
   s->s->StartRestore(fbk,db,pagesize,static_cast<IBPP::BRF>(flags));
   FBSQL_CATCH(return)
}

const char *fbsql_service_wait_msg           (struct fbsql_service *s) {
   FBSQL_TRY
   return s->s->WaitMsg();
   FBSQL_CATCH(return 0)
}

void fbsql_service_wait               (struct fbsql_service *s) {
   FBSQL_TRY
   s->s->Wait();
   FBSQL_CATCH(return)
}

struct fbsql_user_list *fbsql_service_get_user_list(struct fbsql_service *s) {
   fbsql_user_list *result = new fbsql_user_list;
  s->s->GetUsers(result->users);
  return result;
}

void fbsql_service_delete             (struct fbsql_service *s) {
   FBSQL_TRY
   delete s;
   FBSQL_CATCH(return)
}

#define U reinterpret_cast<IBPP::User*>(u)

struct fbsql_user *fbsql_user_new() {
   return reinterpret_cast<struct fbsql_user*>(new IBPP::User);
}

int  fbsql_user_get_name_size       (struct fbsql_user *u) {
   return U->username.size();
}

void fbsql_user_get_name            (struct fbsql_user *u, char *buf) {
   std::copy(U->username.begin(),U->username.end(),buf);
   buf[U->username.size()] = '\0';
}

int  fbsql_user_get_password_size   (struct fbsql_user *u) {
   return U->password.size();
}

void fbsql_user_get_password        (struct fbsql_user *u, char *buf) {
   std::copy(U->password.begin(),U->password.end(),buf);
   buf[U->password.size()] = '\0';
}

int  fbsql_user_get_firstname_size  (struct fbsql_user *u) {
   return U->firstname.size();
}

void fbsql_user_get_firstname       (struct fbsql_user *u, char *buf) {
   std::copy(U->firstname.begin(),U->firstname.end(),buf);
   buf[U->firstname.size()] = '\0';
}

int  fbsql_user_get_middlename_size (struct fbsql_user *u) {
   return U->middlename.size();
}

void fbsql_user_get_middlename      (struct fbsql_user *u, char *buf) {
   std::copy(U->middlename.begin(),U->middlename.end(),buf);
   buf[U->middlename.size()] = '\0';
}

int  fbsql_user_get_lastname_size   (struct fbsql_user *u) {
   return U->lastname.size();
}

void fbsql_user_get_lastname        (struct fbsql_user *u, char *buf) {
   std::copy(U->lastname.begin(),U->lastname.end(),buf);
   buf[U->lastname.size()] = '\0';
}

int  fbsql_user_get_userid          (struct fbsql_user *u) {
   return U->userid;
}

int  fbsql_user_get_groupid         (struct fbsql_user *u) {
   return U->groupid;
}

void fbsql_user_set_name            (struct fbsql_user *u, const char *buf) {
   U->username = buf;
}

void fbsql_user_set_password        (struct fbsql_user *u, const char *buf) {
   U->password = buf;
}

void fbsql_user_set_firstname       (struct fbsql_user *u, const char *buf) {
   U->firstname = buf;
}

void fbsql_user_set_middlename      (struct fbsql_user *u, const char *buf) {
   U->middlename = buf;
}

void fbsql_user_set_lastname        (struct fbsql_user *u, const char *buf) {
   U->lastname = buf;
}

void fbsql_user_set_userid          (struct fbsql_user *u, int userid) {
   U->userid = userid;
}

void fbsql_user_set_groupid         (struct fbsql_user *u, int groupid) {
   U->userid = groupid;
}

void fbsql_user_delete              (struct fbsql_user *u) {
   delete U;
}

unsigned fbsql_user_list_count          (struct fbsql_user_list *users) {
   return users->users.size();
}
struct   fbsql_user *fbsql_user_list_get(struct fbsql_user_list *users, unsigned index) {
   return reinterpret_cast<fbsql_user*>(&users->users[index]);
}

void     fbsql_user_list_delete         (struct fbsql_user_list *users) {
   delete users;
}
