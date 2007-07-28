/* Copyright 2007 Daniel Albuschat
 * See license.txt for further licensing information
 */

#ifndef STATEMENT_H_12748
#define STATEMENT_H_12748

_dllexport_ struct fbsql_statement  *fbsql_statement_new               (struct fbsql_database *db, struct fbsql_transaction *tr);
_dllexport_ void                   fbsql_statement_prepare           (struct fbsql_statement *st, const char *sql);
_dllexport_ void                   fbsql_statement_execute           (struct fbsql_statement *st);
_dllexport_ void                   fbsql_statement_execute_immediate (struct fbsql_statement *st, const char *sql);
_dllexport_ void                   fbsql_statement_cursor_execute    (struct fbsql_statement *st, const char *cursor, const char *sql);
_dllexport_ int                    fbsql_statement_affected_rows     (struct fbsql_statement *st);
_dllexport_ const char            *fbsql_statement_sql               (struct fbsql_statement *st);
_dllexport_ struct fbsql_plan       *fbsql_statement_plan              (struct fbsql_statement *st);
_dllexport_ void                   fbsql_statement_close             (struct fbsql_statement *st);
_dllexport_ int                    fbsql_statement_fetch             (struct fbsql_statement *st);
_dllexport_ int                    fbsql_statement_is_null           (struct fbsql_statement *st, int column);
_dllexport_ int                    fbsql_statement_is_null_by_name   (struct fbsql_statement *st, const char *column);
_dllexport_ int                    fbsql_statement_column_num        (struct fbsql_statement *st, const char *column);
_dllexport_ const char            *fbsql_statement_column_name       (struct fbsql_statement *st, int column);
_dllexport_ const char            *fbsql_statement_column_alias      (struct fbsql_statement *st, int column);
_dllexport_ const char            *fbsql_statement_column_table      (struct fbsql_statement *st, int column);
_dllexport_ enum fbsql_SDT           fbsql_statement_column_type       (struct fbsql_statement *st, int column);
_dllexport_ int                    fbsql_statement_column_subtype    (struct fbsql_statement *st, int column);
_dllexport_ int                    fbsql_statement_column_size       (struct fbsql_statement *st, int column);
_dllexport_ int                    fbsql_statement_column_count      (struct fbsql_statement *st);
_dllexport_ enum fbsql_SDT           fbsql_statement_parameter_type    (struct fbsql_statement *st, int param);
_dllexport_ int                    fbsql_statement_parameter_subtype (struct fbsql_statement *st, int param);
_dllexport_ int                    fbsql_statement_parameter_size    (struct fbsql_statement *st, int param);
_dllexport_ int                    fbsql_statement_parameter_scale   (struct fbsql_statement *st, int param);
_dllexport_ int                    fbsql_statement_parameter_count   (struct fbsql_statement *st);
_dllexport_ int                    fbsql_statement_get_bool          (struct fbsql_statement *st, int column, int *value);
_dllexport_ int                    fbsql_statement_get_short         (struct fbsql_statement *st, int column, int16_t *value);
_dllexport_ int                    fbsql_statement_get_integer       (struct fbsql_statement *st, int column, int32_t *value);
_dllexport_ int                    fbsql_statement_get_longint       (struct fbsql_statement *st, int column, int64_t *value);
_dllexport_ int                    fbsql_statement_get_string_size   (struct fbsql_statement *st, int column);
_dllexport_ int                    fbsql_statement_get_string        (struct fbsql_statement *st, int column, char *value);
_dllexport_ int                    fbsql_statement_get_float         (struct fbsql_statement *st, int column, float *value);
_dllexport_ int                    fbsql_statement_get_double        (struct fbsql_statement *st, int column, double *value);
_dllexport_ struct fbsql_timestamp  *fbsql_statement_get_timestamp     (struct fbsql_statement *st, int column);
_dllexport_ struct fbsql_dbkey      *fbsql_statement_get_dbkey         (struct fbsql_statement *st, int column);
_dllexport_ struct fbsql_blob       *fbsql_statement_get_blob          (struct fbsql_statement *st, int column);
_dllexport_ void                   fbsql_statement_set_null          (struct fbsql_statement *st, int column);
_dllexport_ void                   fbsql_statement_set_bool          (struct fbsql_statement *st, int column, int value);
_dllexport_ void                   fbsql_statement_set_short         (struct fbsql_statement *st, int column, int16_t value);
_dllexport_ void                   fbsql_statement_set_integer       (struct fbsql_statement *st, int column, int32_t value);
_dllexport_ void                   fbsql_statement_set_longint       (struct fbsql_statement *st, int column, int64_t value);
_dllexport_ void                   fbsql_statement_set_string        (struct fbsql_statement *st, int column, const char *value);
_dllexport_ void                   fbsql_statement_set_float         (struct fbsql_statement *st, int column, float value);
_dllexport_ void                   fbsql_statement_set_double        (struct fbsql_statement *st, int column, double value);
_dllexport_ void                   fbsql_statement_set_timestamp     (struct fbsql_statement *st, int column, struct fbsql_timestamp *t);
_dllexport_ void                   fbsql_statement_set_dbkey         (struct fbsql_statement *st, int column, struct fbsql_dbkey     *k);
_dllexport_ void                   fbsql_statement_set_blob          (struct fbsql_statement *st, int column, struct fbsql_blob      *b);
_dllexport_ void                   fbsql_statement_delete            (struct fbsql_statement *st);

/* Timestamp */
_dllexport_ struct fbsql_timestamp *fbsql_timestamp_new();
_dllexport_ struct fbsql_timestamp *fbsql_timestamp_new_time(int hour, int minute, int second, int tenthousands);
_dllexport_ struct fbsql_timestamp *fbsql_timestamp_new_date(int year, int month, int day);
_dllexport_ struct fbsql_timestamp *fbsql_timestamp_new_datetime(
   int year, int month, int day, int hour, int minute, int second, int tenthousands);
_dllexport_ void fbsql_timestamp_set_time(
   struct fbsql_timestamp *ts,
   int hour, int minute, int second, int tenthousands);
_dllexport_ void fbsql_timestamp_set_date(
   struct fbsql_timestamp *ts,
   int year, int month, int day);
_dllexport_ void fbsql_timestamp_set_datetime(
   struct fbsql_timestamp *ts,
   int year, int month, int day, int hour, int minute, int second, int tenthousands);
_dllexport_ void fbsql_timestamp_get_time(
   struct fbsql_timestamp *ts,
   int *hour, int *minute, int *second, int *tenthousands);
_dllexport_ void fbsql_timestamp_get_date(
   struct fbsql_timestamp *ts,
   int *year, int *month, int *day);
_dllexport_ void fbsql_timestamp_get_datetime(
   struct fbsql_timestamp *ts,
   int *year, int *month, int *day, int *hour, int *minute, int *second, int *tenthousands);
_dllexport_ void fbsql_timestamp_clear(struct fbsql_timestamp *ts);
_dllexport_ void fbsql_timestamp_today(struct fbsql_timestamp *ts);
_dllexport_ int fbsql_timestamp_before(struct fbsql_timestamp *lhs, struct fbsql_timestamp *rhs);
_dllexport_ int fbsql_timestamp_equal(struct fbsql_timestamp *lhs, struct fbsql_timestamp *rhs);
_dllexport_ void fbsql_timestamp_now(struct fbsql_timestamp *ts);
_dllexport_ void fbsql_timestamp_delete(struct fbsql_timestamp *ts);

/* Blob */
_dllexport_ struct fbsql_blob *fbsql_blob_new     (struct fbsql_database *db, struct fbsql_transaction *tr);
_dllexport_ void             fbsql_blob_create  (struct fbsql_blob *b);
_dllexport_ void             fbsql_blob_cancel  (struct fbsql_blob *b);
_dllexport_ void             fbsql_blob_open    (struct fbsql_blob *b);
_dllexport_ void             fbsql_blob_close   (struct fbsql_blob *b);
_dllexport_ int              fbsql_blob_read    (struct fbsql_blob *b, void *dest, int size);
_dllexport_ void             fbsql_blob_write   (struct fbsql_blob *b, const void *source, int size);
_dllexport_ void             fbsql_blob_info    (struct fbsql_blob *b, int *size, int *largest, int *segments);
_dllexport_ void             fbsql_blob_save    (struct fbsql_blob *b, const char *data, int size);
_dllexport_ unsigned         fbsql_blob_size    (struct fbsql_blob *b);
_dllexport_ void             fbsql_blob_load    (struct fbsql_blob *b, char *data);
_dllexport_ void             fbsql_blob_freemem (void *mem);
_dllexport_ void             fbsql_blob_delete  (struct fbsql_blob *b);

/* DB-Key */
_dllexport_ struct fbsql_dbkey *fbsql_dbkey_new();
_dllexport_ void fbsql_dbkey_clear(struct fbsql_dbkey *k);
_dllexport_ int fbsql_dbkey_size(struct fbsql_dbkey *k);
_dllexport_ void fbsql_dbkey_set(struct fbsql_dbkey *k, const void *key, int size);
_dllexport_ void fbsql_dbkey_get(struct fbsql_dbkey *k, void *key, int size);
_dllexport_ const char *fbsql_dbkey_as_string(struct fbsql_dbkey *k);
_dllexport_ void fbsql_dbkey_assign(struct fbsql_dbkey *lhs, struct fbsql_dbkey *rhs);
_dllexport_ void fbsql_dbkey_delete(struct fbsql_dbkey *k);

/* Plan */
_dllexport_ const char *fbsql_plan_sql(struct fbsql_plan *plan);
_dllexport_ void fbsql_plan_delete(struct fbsql_plan *plan);

#endif
