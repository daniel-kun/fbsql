# Functions #

### fbsql\_database\_new ###
```
struct fbsql_database *fbsql_database_new(
   const char *host, 
   const char *db, 
   const char *user, 
   const char *passwd, 
   const char *role,
   const char *charset,
   const char *create_params);
```
### fbsql\_database\_connect ###
`void fbsql_database_connect(struct fbsql_database *db);`
### fbsql\_database\_connected ###
```
int fbsql_database_connected(struct fbsql_database *db);
```
### fbsql\_database\_inactivate ###
```
void fbsql_database_inactivate(struct fbsql_database *db);
```
### fbsql\_database\_disconnect ###
```
void fbsql_database_disconnect(struct fbsql_database *db);
```
### fbsql\_database\_create ###
```
void fbsql_database_create(struct fbsql_database *db, int dialect);
```
### fbsql\_database\_drop ###
```
void fbsql_database_drop(struct fbsql_database *db);
```
### fbsql\_database\_statistics ###
```
void fbsql_database_statistics(struct fbsql_database *db, int *fetches, int *marks, int *reads, int *writes);
```
### fbsql\_database\_counts ###
```
void fbsql_database_counts(struct fbsql_database *db, int *ins, int *upd, int *del, int *readidx, int *readseq);
```
### fbsql\_database\_users ###
```
struct fbsql_users *fbsql_database_users(struct fbsql_database *db);
```
Also see ReferenceUsers
### fbsql\_database\_dialect ###
```
int fbsql_database_dialect(struct fbsql_database *db);
```
### fbsql\_database\_servername ###
```
const char * fbsql_database_servername(struct fbsql_database *db);
```
### fbsql\_database\_database ###
```
const char *fbsql_database_database(struct fbsql_database *db);
```
Somewhat misleading name. Returns the database-path you're connected to.
### fbsql\_database\_user ###
```
const char *fbsql_database_user(struct fbsql_database *db);
```
### fbsql\_database\_password ###
```
const char *fbsql_database_password(struct fbsql_database *db);
```
### fbsql\_database\_role ###
```
const char *fbsql_database_role(struct fbsql_database *db);
```
### fbsql\_database\_charset ###
```
const char *fbsql_database_charset(struct fbsql_database *db);
```
### fbsql\_database\_create\_params ###
```
const char *fbsql_database_create_params(struct fbsql_database *db);
```
### fbsql\_database\_create\_params ###
```
void fbsql_database_delete(struct fbsql_database *db);
```