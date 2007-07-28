#include "fbsql_impl.h"

/* Statement */

struct fbsql_statement *fbsql_statement_new(
   struct fbsql_database *db,
   struct fbsql_transaction *tr) {
   FBSQL_TRY
   return new fbsql_statement(db->db,tr->tr);
   FBSQL_CATCH(return 0)
}

void fbsql_statement_prepare(struct fbsql_statement *st, const char *sql) {
   FBSQL_TRY
   st->st->Prepare(sql);
   FBSQL_CATCH(return)
}

void fbsql_statement_execute(struct fbsql_statement *st) {
   FBSQL_TRY
   st->st->Execute();
   FBSQL_CATCH(return)
}

void fbsql_statement_execute_immediate(struct fbsql_statement *st, const char *sql) {
   FBSQL_TRY
   st->st->ExecuteImmediate(sql);
   FBSQL_CATCH(return)
}

void fbsql_statement_cursor_execute(struct fbsql_statement *st, const char *cursor, const char *sql) {
   FBSQL_TRY
   if( sql )
      st->st->CursorExecute(cursor,sql);
   else
      st->st->CursorExecute(cursor);
   FBSQL_CATCH(return)
}

int fbsql_statement_affected_rows(struct fbsql_statement *st) {
   FBSQL_TRY
   return st->st->AffectedRows();
   FBSQL_CATCH(return 0)
}

const char *fbsql_statement_sql(struct fbsql_statement *st) {
   FBSQL_TRY
   return st->st->Sql().c_str();
   FBSQL_CATCH(return 0)
}

struct fbsql_plan *fbsql_statement_plan(struct fbsql_statement *st) {
   FBSQL_TRY
   fbsql_plan *p = new fbsql_plan;
   std::string plan;
   st->st->Plan(plan);
   p->plan = new char[plan.size()+1];
   std::copy(plan.begin(),plan.end(),p->plan);
   p->plan[plan.size()] = '\0';
   return p;
   FBSQL_CATCH(return 0)
}

const char *fbsql_plan_sql(struct fbsql_plan *plan) {
   FBSQL_TRY
   return plan->plan;
   FBSQL_CATCH(return 0)
}

void fbsql_plan_delete(struct fbsql_plan *plan) {
   FBSQL_TRY
   delete plan;
   FBSQL_CATCH(return)
}

void fbsql_statement_close(struct fbsql_statement *st) {
   FBSQL_TRY
   st->st->Close();
   FBSQL_CATCH(return)
}

int fbsql_statement_fetch(struct fbsql_statement *st) {
   FBSQL_TRY
   return st->st->Fetch();
   FBSQL_CATCH(return 0)
}

int fbsql_statement_is_null(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->IsNull(column);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_is_null_by_name(struct fbsql_statement *st, const char *column) {
   FBSQL_TRY
   return st->st->IsNull(column);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_column_num(struct fbsql_statement *st, const char *column) {
   FBSQL_TRY
   return st->st->ColumnNum(column);
   FBSQL_CATCH(return 0)
}

const char *fbsql_statement_column_name(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->ColumnName(column);
   FBSQL_CATCH(return 0)
}

const char *fbsql_statement_column_alias(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->ColumnAlias(column);
   FBSQL_CATCH(return 0)
}

const char *fbsql_statement_column_table(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->ColumnTable(column);
   FBSQL_CATCH(return 0)
}

enum fbsql_SDT fbsql_statement_column_type(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return (fbsql_SDT)st->st->ColumnType(column);
   FBSQL_CATCH(return (fbsql_SDT)0)
}

int fbsql_statement_column_subtype(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->ColumnSubtype(column);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_column_size(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   return st->st->ColumnSize(column);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_column_count(struct fbsql_statement *st) {
   FBSQL_TRY
   return st->st->Columns();
   FBSQL_CATCH(return 0)
}

enum fbsql_SDT fbsql_statement_parameter_type(struct fbsql_statement *st, int param) {
   FBSQL_TRY
   return (fbsql_SDT)st->st->ParameterType(param);
   FBSQL_CATCH(return (fbsql_SDT)0)
}

int fbsql_statement_parameter_subtype(struct fbsql_statement *st, int param) {
   FBSQL_TRY
   return st->st->ParameterSubtype(param);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_parameter_size(struct fbsql_statement *st, int param) {
   FBSQL_TRY
   return st->st->ParameterSize(param);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_parameter_scale(struct fbsql_statement *st, int param) {
   FBSQL_TRY
   return st->st->ParameterScale(param);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_parameter_count(struct fbsql_statement *st) {
   FBSQL_TRY
   return st->st->Parameters();
   FBSQL_CATCH(return 0)
}

void fbsql_statement_delete(struct fbsql_statement *st) {
   FBSQL_TRY
   delete st;
   FBSQL_CATCH(return)
}

/* Timestamp */

struct fbsql_timestamp *fbsql_timestamp_new() {
   FBSQL_TRY
   return new fbsql_timestamp;
   FBSQL_CATCH(return 0)
}

struct fbsql_timestamp *fbsql_timestamp_new_time(int hour, int minute, int second, int tenthousands) {
   FBSQL_TRY
   fbsql_timestamp *ts = new fbsql_timestamp;
   ts->ts = IBPP::Time(hour,minute,second,tenthousands);
   return ts;
   FBSQL_CATCH(return 0)
}

struct fbsql_timestamp *fbsql_timestamp_new_date(int year, int month, int day) {
   FBSQL_TRY
   fbsql_timestamp *ts = new fbsql_timestamp;
   ts->ts = IBPP::Date(year,month,day);
   return ts;
   FBSQL_CATCH(return 0)
}

struct fbsql_timestamp *fbsql_timestamp_new_datetime(
   int year, int month, int day, int hour, int minute, int second, int tenthousands) {
   FBSQL_TRY
   fbsql_timestamp *ts = new fbsql_timestamp;
   ts->ts = IBPP::Timestamp(year,month,day,hour,minute,second,tenthousands);
   return ts;
   FBSQL_CATCH(return 0)
}

void fbsql_timestamp_set_time(
   struct fbsql_timestamp *ts,
   int hour, int minute, int second, int tenthousands) {
   FBSQL_TRY
   ts->ts = IBPP::Time(hour,minute,second,tenthousands);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_set_date(
   struct fbsql_timestamp *ts,
   int year, int month, int day) {
   FBSQL_TRY
   ts->ts = IBPP::Date(year,month,day);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_set_datetime(
   struct fbsql_timestamp *ts,
   int year, int month, int day, int hour, int minute, int second, int tenthousands) {
   FBSQL_TRY
   ts->ts = IBPP::Timestamp(year,month,day,hour,minute,second,tenthousands);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_get_time(
   struct fbsql_timestamp *ts,
   int *hour, int *minute, int *second, int *tenthousands) {
   FBSQL_TRY
   ts->ts.GetTime(*hour,*minute,*second,*tenthousands);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_get_date(
   struct fbsql_timestamp *ts,
   int *year, int *month, int *day) {
   FBSQL_TRY
   ts->ts.GetDate(*year,*month,*day);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_get_datetime(
   struct fbsql_timestamp *ts,
   int *year, int *month, int *day, int *hour, int *minute, int *second, int *tenthousands) {
   FBSQL_TRY
   fbsql_timestamp_get_date(ts,year,month,day);
   fbsql_timestamp_get_time(ts,hour,minute,second,tenthousands);
   FBSQL_CATCH(return)
}

void fbsql_timestamp_clear(struct fbsql_timestamp *ts) {
   FBSQL_TRY
   ts->ts.Clear();
   FBSQL_CATCH(return)
}

void fbsql_timestamp_today(struct fbsql_timestamp *ts) {
   FBSQL_TRY
   ts->ts.Today();
   FBSQL_CATCH(return)
}

int fbsql_timestamp_before(struct fbsql_timestamp *lhs, struct fbsql_timestamp *rhs) {
   FBSQL_TRY
   return lhs->ts < rhs->ts;
   FBSQL_CATCH(return 0)
}

int fbsql_timestamp_equal(struct fbsql_timestamp *lhs, struct fbsql_timestamp *rhs) {
   FBSQL_TRY
   return lhs->ts == rhs->ts;
   FBSQL_CATCH(return 0)
}

void fbsql_timestamp_now(struct fbsql_timestamp *ts) {
   FBSQL_TRY
   ts->ts.Now();
   FBSQL_CATCH(return)
}

void fbsql_timestamp_delete(struct fbsql_timestamp *ts) {
   FBSQL_TRY
   delete ts;
   FBSQL_CATCH(return)
}

/* Blob */

struct fbsql_blob *fbsql_blob_new(struct fbsql_database *db, struct fbsql_transaction *tr) {
   FBSQL_TRY
   return new fbsql_blob(db->db,tr->tr);
   FBSQL_CATCH(return 0)
}

void fbsql_blob_create(struct fbsql_blob *b) {
   FBSQL_TRY
   b->b->Create();
   FBSQL_CATCH(return)
}

void fbsql_blob_cancel(struct fbsql_blob *b) {
   FBSQL_TRY
   b->b->Cancel();
   FBSQL_CATCH(return)
}

void fbsql_blob_open(struct fbsql_blob *b) {
   FBSQL_TRY
   b->b->Open();
   FBSQL_CATCH(return)
}

void fbsql_blob_close(struct fbsql_blob *b) {
   FBSQL_TRY
   b->b->Close();
   FBSQL_CATCH(return)
}

int fbsql_blob_read(struct fbsql_blob *b, void *dest, int size) {
   FBSQL_TRY
   return b->b->Read(dest,size);
   FBSQL_CATCH(return 0)
}

void fbsql_blob_write(struct fbsql_blob *b, const void *source, int size) {
   FBSQL_TRY
   b->b->Write(source,size);
   FBSQL_CATCH(return)
}

void fbsql_blob_info(struct fbsql_blob *b, int *size, int *largest, int *segments) {
   FBSQL_TRY
   b->b->Info(size,largest,segments);
   FBSQL_CATCH(return)
}

void fbsql_blob_save(struct fbsql_blob *b, const char *data, int size) {
   FBSQL_TRY
   b->b->Save(std::string(data,data+size));
   FBSQL_CATCH(return)
}

unsigned fbsql_blob_size(struct fbsql_blob *b) {
   FBSQL_TRY
   int size, largest, segments;
   b->b->Open();
   b->b->Info(&size,&largest,&segments);
   b->b->Close();
   return static_cast<unsigned>(size);
   FBSQL_CATCH(return 0)
}

void fbsql_blob_load(struct fbsql_blob *b, char *data) {
   FBSQL_TRY
   std::string s;
   b->b->Load(s);
   std::copy(s.begin(),s.end(),data);
   data[s.size()] = '\0';
   FBSQL_CATCH(return)
}

void fbsql_blob_freemem(void *mem) {
   FBSQL_TRY
   free(mem);
   FBSQL_CATCH(return)
}

void fbsql_blob_delete(struct fbsql_blob *b) {
   FBSQL_TRY
   delete b;
   FBSQL_CATCH(return)
}

struct fbsql_dbkey *fbsql_dbkey_new() {
   FBSQL_TRY
   return new fbsql_dbkey;
   FBSQL_CATCH(return 0)
}

void fbsql_dbkey_clear(struct fbsql_dbkey *k) {
   FBSQL_TRY
   k->k.Clear();
   FBSQL_CATCH(return)
}

int fbsql_dbkey_size(struct fbsql_dbkey *k) {
   FBSQL_TRY
   return k->k.Size();
   FBSQL_CATCH(return 0)
}

void fbsql_dbkey_set(struct fbsql_dbkey *k, const void *key, int size) {
   FBSQL_TRY
   k->k.SetKey(key,size);
   FBSQL_CATCH(return)
}

void fbsql_dbkey_get(struct fbsql_dbkey *k, void *key, int size) {
   FBSQL_TRY
   k->k.GetKey(key,size);
   FBSQL_CATCH(return)
}

const char *fbsql_dbkey_as_string(struct fbsql_dbkey *k) {
   FBSQL_TRY
   return k->k.AsString();
   FBSQL_CATCH(return 0)
}

void fbsql_dbkey_assign(struct fbsql_dbkey *lhs, struct fbsql_dbkey *rhs) {
   FBSQL_TRY
   lhs->k = rhs->k;
   FBSQL_CATCH(return)
}

void fbsql_dbkey_delete(struct fbsql_dbkey *k) {
   FBSQL_TRY
   delete k;
   FBSQL_CATCH(return)
}

int fbsql_statement_get_bool(struct fbsql_statement *st, int column, int *value) {
   FBSQL_TRY
   bool b, result;
   result = st->st->Get(column,b);
   *value = b;
   return result;
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_short(struct fbsql_statement *st, int column, int16_t *value) {
   FBSQL_TRY
   return st->st->Get(column,*value);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_integer(struct fbsql_statement *st, int column, int32_t *value) {
   FBSQL_TRY
   return st->st->Get(column,*value);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_longint(struct fbsql_statement *st, int column, int64_t *value) {
   FBSQL_TRY
   return st->st->Get(column,*value);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_string_size(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   std::string str;
   if( st->st->Get(column,str) )
      return 0;
   else
      return static_cast<int>(str.size())+1;
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_string(struct fbsql_statement *st, int column, char *value) {
   FBSQL_TRY
   std::string str;
   bool result = st->st->Get(column,str);
   std::size_t stmp(str.size());
   std::copy(str.begin(),str.end(),value);
   value[stmp] = '\0';
   return result;
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_float(struct fbsql_statement *st, int column, float *value) {
   FBSQL_TRY
   return st->st->Get(column,*value);
   FBSQL_CATCH(return 0)
}

int fbsql_statement_get_double(struct fbsql_statement *st, int column, double *value) {
   FBSQL_TRY
   return st->st->Get(column,*value);
   FBSQL_CATCH(return 0)
}

struct fbsql_timestamp *fbsql_statement_get_timestamp(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   IBPP::Timestamp ts;
   if( !st->st->Get(column,ts) ) {
      fbsql_timestamp *result = new fbsql_timestamp;
      result->ts = ts;
      return result;
   }
   return 0;
   FBSQL_CATCH(return 0)
}

struct fbsql_dbkey *fbsql_statement_get_dbkey(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   IBPP::DBKey k;
   if( !st->st->Get(column,k) ) {
      fbsql_dbkey *result = new fbsql_dbkey;
      result->k = k;
      return result;
   }
   return 0;
   FBSQL_CATCH(return 0)
}

struct fbsql_blob *fbsql_statement_get_blob(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   IBPP::Blob b = IBPP::BlobFactory(st->st->DatabasePtr(),st->st->TransactionPtr());
   if( !st->st->Get(column,b) ) {
      fbsql_blob *result = new fbsql_blob;
      result->b = b;
      return result;
   }
   return 0;
   FBSQL_CATCH(return 0)
}

void fbsql_statement_set_bool(struct fbsql_statement *st, int column) {
   FBSQL_TRY
   st->st->SetNull(column);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_bool(struct fbsql_statement *st, int column, int value) {
   FBSQL_TRY
   bool b(value);
   st->st->Set(column,b);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_short(struct fbsql_statement *st, int column, int16_t value) {
   FBSQL_TRY
   st->st->Set(column,value);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_integer(struct fbsql_statement *st, int column, int32_t value) {
   FBSQL_TRY
   st->st->Set(column,value);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_longint(struct fbsql_statement *st, int column, int64_t value) {
   FBSQL_TRY
   st->st->Set(column,value);
   FBSQL_CATCH(return)
}

void  fbsql_statement_set_string(struct fbsql_statement *st, int column, const char *value) {
   st->st->Set(column,value);
}

void fbsql_statement_set_float(struct fbsql_statement *st, int column, float value) {
   FBSQL_TRY
   st->st->Set(column,value);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_double(struct fbsql_statement *st, int column, double value) {
   FBSQL_TRY
   st->st->Set(column,value);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_timestamp(struct fbsql_statement *st, int column, struct fbsql_timestamp *t) {
   FBSQL_TRY
   st->st->Set(column,t->ts);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_dbkey(struct fbsql_statement *st, int column, struct fbsql_dbkey *k) {
   FBSQL_TRY
   st->st->Set(column,k->k);
   FBSQL_CATCH(return)
}

void fbsql_statement_set_blob(struct fbsql_statement *st, int column, struct fbsql_blob *b) {
   FBSQL_TRY
   st->st->Set(column,b->b);
   FBSQL_CATCH(return)
}
