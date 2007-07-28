#ifndef SERVICE_H_89323
#define SERVICE_H_89323

struct fbsql_user;

_dllexport_ struct fbsql_service *fbsql_service_new(
      const char *server,
      const char *user,
      const char *password);
_dllexport_ void fbsql_service_connect            (struct fbsql_service *s);
_dllexport_ int  fbsql_service_connected          (struct fbsql_service *s);
_dllexport_ void fbsql_service_disconnect         (struct fbsql_service *s);
_dllexport_ int  fbsql_service_get_version_size   (struct fbsql_service *s);
_dllexport_ void fbsql_service_get_version        (struct fbsql_service *s, char *buf);
_dllexport_ void fbsql_service_add_user           (struct fbsql_service *s, struct fbsql_user *u);
_dllexport_ void fbsql_service_get_user           (struct fbsql_service *s, struct fbsql_user *u);
_dllexport_ void fbsql_service_modify_user        (struct fbsql_service *s, struct fbsql_user *u);
_dllexport_ void fbsql_service_remove_user        (struct fbsql_service *s, struct fbsql_user *u);
_dllexport_ void fbsql_service_set_page_buffers   (struct fbsql_service *s, const char *db, int buffers);
_dllexport_ void fbsql_service_set_sweep_interval (struct fbsql_service *s, const char *db, int interval);
_dllexport_ void fbsql_service_set_sync_write     (struct fbsql_service *s, const char *db, int sync);
_dllexport_ void fbsql_service_set_read_only      (struct fbsql_service *s, const char *db, int readonly);
_dllexport_ void fbsql_service_set_reserve_space  (struct fbsql_service *s, const char *db, int reserve);
_dllexport_ void fbsql_service_shutdown           (struct fbsql_service *s, const char *db, enum fbsql_DSM mode, int timeout);
_dllexport_ void fbsql_service_restart            (struct fbsql_service *s, const char *db);
_dllexport_ void fbsql_service_sweep              (struct fbsql_service *s, const char *db);
_dllexport_ void fbsql_service_repair             (struct fbsql_service *s, const char *db, enum fbsql_RPF flags);
_dllexport_ void fbsql_service_start_backup       (struct fbsql_service *s, const char *db, const char *fbk, enum fbsql_BRF flags);
_dllexport_ void fbsql_service_start_restore      (struct fbsql_service *s, const char *fbk, const char *db, int pagesize, enum fbsql_BRF flags);
_dllexport_ const char *fbsql_service_wait_msg    (struct fbsql_service *s);
_dllexport_ void fbsql_service_wait               (struct fbsql_service *s);
_dllexport_ struct fbsql_user_list *fbsql_service_get_user_list      (struct fbsql_service *s);
_dllexport_ void fbsql_service_delete             (struct fbsql_service *s);

_dllexport_ struct fbsql_user *fbsql_user_new();
_dllexport_ int  fbsql_user_get_name_size       (struct fbsql_user *u);
_dllexport_ void fbsql_user_get_name            (struct fbsql_user *u, char *buf);
_dllexport_ int  fbsql_user_get_password_size   (struct fbsql_user *u);
_dllexport_ void fbsql_user_get_password        (struct fbsql_user *u, char *buf);
_dllexport_ int  fbsql_user_get_firstname_size  (struct fbsql_user *u);
_dllexport_ void fbsql_user_get_firstname       (struct fbsql_user *u, char *buf);
_dllexport_ int  fbsql_user_get_middlename_size (struct fbsql_user *u);
_dllexport_ void fbsql_user_get_middlename      (struct fbsql_user *u, char *buf);
_dllexport_ int  fbsql_user_get_lastname_size   (struct fbsql_user *u);
_dllexport_ void fbsql_user_get_lastname        (struct fbsql_user *u, char *buf);
_dllexport_ int  fbsql_user_get_userid          (struct fbsql_user *u);
_dllexport_ int  fbsql_user_get_groupid         (struct fbsql_user *u);
_dllexport_ void fbsql_user_set_name            (struct fbsql_user *u, const char *buf);
_dllexport_ void fbsql_user_set_password        (struct fbsql_user *u, const char *buf);
_dllexport_ void fbsql_user_set_firstname       (struct fbsql_user *u, const char *buf);
_dllexport_ void fbsql_user_set_middlename      (struct fbsql_user *u, const char *buf);
_dllexport_ void fbsql_user_set_lastname        (struct fbsql_user *u, const char *buf);
_dllexport_ void fbsql_user_set_userid          (struct fbsql_user *u, int userid);
_dllexport_ void fbsql_user_set_groupid         (struct fbsql_user *u, int groupid);
_dllexport_ void fbsql_user_delete              (struct fbsql_user *u);

_dllexport_ unsigned             fbsql_user_list_count (struct fbsql_user_list *users);
_dllexport_ struct   fbsql_user *fbsql_user_list_get   (struct fbsql_user_list *users, unsigned index);
_dllexport_ void                 fbsql_user_list_delete(struct fbsql_user_list *users);

#endif
