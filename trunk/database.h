#ifndef DATABASE_H_89324
#define DATABASE_H_89324

_dllexport_ struct fbsql_database *fbsql_database_new(
   const char *host, 
   const char *db, 
   const char *user, 
   const char *passwd, 
   const char *role,
   const char *charset,
   const char *create_params);
_dllexport_ void              fbsql_database_connect      (struct fbsql_database *db);
_dllexport_ int               fbsql_database_connected    (struct fbsql_database *db);
_dllexport_ void              fbsql_database_inactivate   (struct fbsql_database *db);
_dllexport_ void              fbsql_database_disconnect   (struct fbsql_database *db);
_dllexport_ void              fbsql_database_create       (struct fbsql_database *db, int dialect);
_dllexport_ void              fbsql_database_drop         (struct fbsql_database *db);
_dllexport_ void              fbsql_database_statistics   (struct fbsql_database *db, int *fetches, int *marks, int *reads, int *writes);
_dllexport_ void              fbsql_database_counts       (struct fbsql_database *db, int *ins, int *upd, int *del, int *readidx, int *readseq);
_dllexport_ struct fbsql_users *fbsql_database_users        (struct fbsql_database *db);
_dllexport_ int               fbsql_database_dialect      (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_servername   (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_database     (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_user         (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_password     (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_role         (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_charset      (struct fbsql_database *db);
_dllexport_ const char *      fbsql_database_create_params(struct fbsql_database *db);
_dllexport_ void              fbsql_database_delete       (struct fbsql_database *db);

_dllexport_ unsigned    fbsql_users_count (struct fbsql_users *us);
_dllexport_ const char *fbsql_users_get   (struct fbsql_users *us, unsigned index);
_dllexport_ void        fbsql_users_delete(struct fbsql_users *us);

#endif
