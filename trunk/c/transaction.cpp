#include "fbsql_impl.h"

extern "C" {

struct fbsql_transaction *fbsql_transaction_new(
   struct fbsql_database *db,
   fbsql_TAM transaction_access_mode,
   fbsql_TIL transaction_isolation_level,
   fbsql_TLR transaction_lock_resolution,
   fbsql_TFF transaction_factory_flags) {
   FBSQL_TRY
   return new fbsql_transaction(
      db->db,
      (IBPP::TAM)transaction_access_mode,
      (IBPP::TIL)transaction_isolation_level,
      (IBPP::TLR)transaction_lock_resolution,
      (IBPP::TFF)transaction_factory_flags);
   FBSQL_CATCH(return 0)
}

struct fbsql_transaction *fbsql_transaction_new_with_defaults(struct fbsql_database *db) {
   FBSQL_TRY
   return new fbsql_transaction(db->db);
   FBSQL_CATCH(return 0)
}

void fbsql_transaction_start(struct fbsql_transaction *tr) {
   FBSQL_TRY
   tr->tr->Start();
   FBSQL_CATCH(return)
}

int fbsql_transaction_started(struct fbsql_transaction *tr) {
   FBSQL_TRY
   return tr->tr->Started();
   FBSQL_CATCH(return 0)
}

void fbsql_transaction_rollback(struct fbsql_transaction *tr) {
   FBSQL_TRY
   tr->tr->Rollback();
   FBSQL_CATCH(return)
}

void fbsql_transaction_commit(struct fbsql_transaction *tr) {
   FBSQL_TRY
   tr->tr->Commit();
   FBSQL_CATCH(return)
}

void fbsql_transaction_commit_retaining(struct fbsql_transaction *tr) {
   FBSQL_TRY
   tr->tr->CommitRetain();
   FBSQL_CATCH(return)
}

void fbsql_transaction_add_reservation(
   struct fbsql_transaction *tr, 
   struct fbsql_database *db, 
   const char *table,
   fbsql_TTR transaction_table_reservation) {
   FBSQL_TRY
   tr->tr->AddReservation(db->db,table,(IBPP::TTR)transaction_table_reservation);
   FBSQL_CATCH(return)
}

void fbsql_transaction_attach_database(
   struct fbsql_transaction *tr, 
   struct fbsql_database *db,
   fbsql_TAM transaction_access_mode,
   fbsql_TIL transaction_isolation_level,
   fbsql_TLR transaction_lock_resolution,
   fbsql_TFF transaction_factory_flags) {
   FBSQL_TRY
   tr->tr->AttachDatabase(
      db->db,
      (IBPP::TAM)transaction_access_mode,
      (IBPP::TIL)transaction_isolation_level,
      (IBPP::TLR)transaction_lock_resolution,
      (IBPP::TFF)transaction_factory_flags);
   FBSQL_CATCH(return)
}

void fbsql_transaction_detach_database(struct fbsql_transaction *tr, struct fbsql_database *db) {
   FBSQL_TRY
   tr->tr->DetachDatabase(db->db);
   FBSQL_CATCH(return)
}

void fbsql_transaction_delete(struct fbsql_transaction *tr) {
   FBSQL_TRY
   delete tr;
   FBSQL_CATCH(return)
}

}
