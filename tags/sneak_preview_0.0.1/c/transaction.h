/* Copyright 2007 Daniel Albuschat
 * See license.txt for further licensing information
 */

#ifndef TRANSACTION_H_32367
#define TRANSACTION_H_32367

_dllexport_ struct fbsql_transaction *fbsql_transaction_new(
   struct fbsql_database *db,
   enum fbsql_TAM transaction_access_mode,
   enum fbsql_TIL transaction_isolation_level,
   enum fbsql_TLR transaction_lock_resolution,
   enum fbsql_TFF transaction_factory_flags);
_dllexport_ struct fbsql_transaction *fbsql_transaction_new_with_defaults(struct fbsql_database *db);
_dllexport_ void fbsql_transaction_start(struct fbsql_transaction *tr);
_dllexport_ int fbsql_transaction_started(struct fbsql_transaction *tr);
_dllexport_ void fbsql_transaction_rollback(struct fbsql_transaction *tr);
_dllexport_ void fbsql_transaction_commit(struct fbsql_transaction *tr);
_dllexport_ void fbsql_transaction_commit_retaining(struct fbsql_transaction *tr);
_dllexport_ void fbsql_transaction_add_reservation(
   struct fbsql_transaction *tr, 
   struct fbsql_database *db, 
   const char *table,
   enum fbsql_TTR transaction_table_reservation);
_dllexport_ void fbsql_transaction_attach_database(
   struct fbsql_transaction *tr, 
   struct fbsql_database *db,
   enum fbsql_TAM transaction_access_mode,
   enum fbsql_TIL transaction_isolation_level,
   enum fbsql_TLR transaction_lock_resolution,
   enum fbsql_TFF transaction_factory_flags);
_dllexport_ void fbsql_transaction_detach_database(struct fbsql_transaction *tr, struct fbsql_database *db);
_dllexport_ void fbsql_transaction_delete(struct fbsql_transaction *tr);

#endif
