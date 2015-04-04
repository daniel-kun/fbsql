# Users #
Represents a list of multiple users, as returned from fbsql\_database\_get\_users.
## fbsql\_users\_count ##
```
unsigned fbsql_users_count(struct fbsql_users *us);
```
### fbsql\_users\_get ###
```
const char *fbsql_users_get(struct fbsql_users *us, unsigned index);
```
### fbsql\_users\_delete ###
```
void fbsql_users_delete(struct fbsql_users *us);
```