(define-foreign-library libfbsql
  (t (:default "libfbsql")))

(use-foreign-library libfbsql)

(defmacro with-foreign-strings (bindings &rest body)
 "Lets you bind multiple variables as foreign strings and executes 'body'. Recursively uses with-foreign-string."
  (if bindings
      `(with-foreign-string ,(car bindings)
         (with-foreign-strings ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-foreign-pointers (bindings &rest body)
 "Lets you bind multiple variables as foreign pointers and executes 'body'. Recursively uses with-foreign-pointer."
  (if bindings
      `(with-foreign-pointer ,(car bindings)
         (with-foreign-pointers ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defmacro fbsql-test-db-str-result (func name var)
 "Convenience-function to print the result of a binded fbsql-function that returns a string"
 `(format t "~a: ~t~a~%" ,name (foreign-string-to-lisp (,func db))))

(defun cenum-or (enum-type &rest rest)
 (let ((result 0))
  (loop for r in rest do (setf result (logior (foreign-enum-value enum-type r) result)))
  result))

(defmacro test-forms (&body forms)
 `(progn ,@(let ((result (gensym)))
       (loop for f in forms collect 
       `(progn 
          (format t "Testing \"~a\": ~t~t" ',f) (let ((,result ,f)) (format t "~a~%" ,result) ,result))))))

;;;; CFFI bindings for fbsql_database functions
(defmacro defcfun-export (function result &rest parameter)
 `(export (defcfun ,function ,result ,@parameter)))

(defcfun-export "fbsql_database_new" :pointer
  (host :string)
  (db :string)
  (user :string)
  (passwd :string)
  (role :string)
  (charset :string)
  (create-params :string))
(defcfun-export "fbsql_database_new_default"   :pointer (dbpath :string))
(defcfun-export "fbsql_database_connect"       :void    (db :pointer))
(defcfun-export "fbsql_database_connected"     :uint32   (db :pointer))
(defcfun-export "fbsql_database_inactivate"    :void    (db :pointer))
(defcfun-export "fbsql_database_disconnect"    :void    (db :pointer))
(defcfun-export "fbsql_database_create"        :void    (db :pointer) (dialect :uint32))
(defcfun-export "fbsql_database_drop"          :void    (db :pointer))
(defcfun-export "fbsql_database_statistics"    :void    (db :pointer) (fetches :pointer) (marks :pointer) (reads :pointer) (writes :pointer))
(defcfun-export "fbsql_database_counts"        :void    (db :pointer) (inserts :pointer) (updates :pointer) (deletes :pointer) (read-indexes :pointer) (read-sequential :pointer))
(defcfun-export "fbsql_database_users"         :pointer (db :pointer))
(defcfun-export "fbsql_database_dialect"       :uint32   (db :pointer))
(defcfun-export "fbsql_database_servername"    :pointer (db :pointer))
(defcfun-export "fbsql_database_database"      :pointer (db :pointer))
(defcfun-export "fbsql_database_user"          :pointer (db :pointer))
(defcfun-export "fbsql_database_password"      :pointer (db :pointer))
(defcfun-export "fbsql_database_role"          :pointer (db :pointer))
(defcfun-export "fbsql_database_charset"       :pointer (db :pointer))
(defcfun-export "fbsql_database_create_params" :pointer (db :pointer))
(defcfun-export "fbsql_database_delete"        :pointer (db :pointer))

;;;; fbsql_users functions
(defcfun-export "fbsql_users_count"  :unsigned-int (users :pointer))
(defcfun-export "fbsql_users_get"    :string       (users :pointer) (index :unsigned-int))
(defcfun-export "fbsql_users_delete" :void         (users :pointer))

;;;; fbsql_transaction functions and enums
(defcenum fbsql-transaction-access-mode :write :read)
(defcenum fbsql-transaction-isolation-level :concurrency :read-dirty :read-commited :consistency)
(defcenum fbsql-transaction-lock-resolution :wait :nowait)
(defcenum fbsql-transaction-factory-flags (:ignore-limbo 1) (:auto-commit 2) (:no-auto-undo 4))
(defcenum fbsql-transaction-table-reservation :shared-write :shared-read :protected-write :protected-read)

(defcfun-export "fbsql_transaction_new"              :pointer (db :pointer) 
 (access-mode fbsql-transaction-access-mode)
 (isolation-level fbsql-transaction-isolation-level)
 (lock-resolution fbsql-transaction-lock-resolution)
 (factory-flags fbsql-transaction-factory-flags))
(defcfun-export "fbsql_transaction_new_with_defaults" :pointer (db :pointer))
(defcfun-export "fbsql_transaction_start"             :void (tr :pointer))
(defcfun-export "fbsql_transaction_started"           :uint32 (tr :pointer))
(defcfun-export "fbsql_transaction_rollback"          :void (tr :pointer))
(defcfun-export "fbsql_transaction_commit"            :void (tr :pointer))
(defcfun-export "fbsql_transaction_commit_retaining"  :void (tr :pointer))
(defcfun-export "fbsql_transaction_add_reservation"   :void 
 (tr :pointer) 
 (db :pointer) 
 (table :string) 
 (reservation fbsql-transaction-table-reservation))
(defcfun-export "fbsql_transaction_attach_database"   :void
 (tr :pointer)
 (db :pointer) 
 (access-mode fbsql-transaction-access-mode)
 (isolation-level fbsql-transaction-isolation-level)
 (lock-resolution fbsql-transaction-lock-resolution)
 (factory-flags fbsql-transaction-factory-flags))
(defcfun-export "fbsql_transaction_detach_database"   :void (tr :pointer) (db :pointer))
(defcfun-export "fbsql_transaction_delete"            :void (tr :pointer))

;;;; fbsql_statement functions and enums
(defcenum fbsql-data-type :array :blob :date :time :timestamp :string 
 :smallint :uinteger :largeint :float :double)

(defcfun-export "fbsql_statement_new"                           :pointer          (db :pointer) (tr :pointer))
(defcfun-export "fbsql_statement_prepare"                       :void             (st :pointer) (sql :string))
(defcfun-export "fbsql_statement_execute"                       :void             (st :pointer))
(defcfun-export "fbsql_statement_execute_immediate"             :void             (st :pointer) (sql :string))
(defcfun-export "fbsql_statement_affected_rows"                 :uint32            (st :pointer))
(defcfun-export "fbsql_statement_sql"                           :string           (st :pointer))
(defcfun-export "fbsql_statement_plan"                          :string           (st :pointer))
(defcfun-export "fbsql_statement_close"                         :void             (st :pointer))
(defcfun-export "fbsql_statement_fetch"                         :uint32            (st :pointer))
(defcfun-export "fbsql_statement_is_null"                       :uint32            (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_is_null_by_name"               :uint32            (st :pointer) (column :string))
(defcfun-export "fbsql_statement_column_num"                    :uint32            (st :pointer) (column :string))
(defcfun-export "fbsql_statement_column_name"                   :string           (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_alias"                  :string           (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_table"                  :string           (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_type"                   fbsql-data-type   (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_subtype"                :uint32            (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_size"                   :uint32            (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_column_count"                  :uint32            (st :pointer))
(defcfun-export "fbsql_statement_parameter_type"                fbsql-data-type   (st :pointer) (parameter :uint32))
(defcfun-export "fbsql_statement_parameter_subtype"             :uint32            (st :pointer) (parameter :uint32))
(defcfun-export "fbsql_statement_parameter_size"                :uint32            (st :pointer) (parameter :uint32))
(defcfun-export "fbsql_statement_parameter_scale"               :uint32            (st :pointer) (parameter :uint32))
(defcfun-export "fbsql_statement_parameter_count"               :uint32            (st :pointer))
(defcfun-export "fbsql_statement_get_bool"                      :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_short"                     :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_integer"                   :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_longint"                   :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_string_size"               :uint32            (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_get_string"                    :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_float"                     :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_double"                    :uint32            (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_get_timestamp"                 :pointer          (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_get_dbkey"                     :pointer          (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_get_blob"                      :pointer          (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_set_null"                      :void             (st :pointer) (column :uint32))
(defcfun-export "fbsql_statement_set_bool"                      :void             (st :pointer) (column :uint32) (value :uint32))
(defcfun-export "fbsql_statement_set_short"                     :void             (st :pointer) (column :uint32) (value :uint16))
(defcfun-export "fbsql_statement_set_integer"                   :void             (st :pointer) (column :uint32) (value :uint32))
(defcfun-export "fbsql_statement_set_longint"                   :void             (st :pointer) (column :uint32) (value :uint64))
(defcfun-export "fbsql_statement_set_string"                    :void             (st :pointer) (column :uint32) (value :string))
(defcfun-export "fbsql_statement_set_float"                     :void             (st :pointer) (column :uint32) (value :float))
(defcfun-export "fbsql_statement_set_double"                    :void             (st :pointer) (column :uint32) (value :double))
(defcfun-export "fbsql_statement_set_timestamp"                 :void             (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_set_dbkey"                     :void             (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_set_blob"                      :void             (st :pointer) (column :uint32) (value :pointer))
(defcfun-export "fbsql_statement_delete"                        :void             (st :pointer))

;;;; fbsql_timestamp functions
(defcfun-export "fbsql_timestamp_new"          :pointer)
(defcfun-export "fbsql_timestamp_new_time"     :pointer (hour :uint32) (minute :uint32) (second :uint32) (tenthousands :uint32))
(defcfun-export "fbsql_timestamp_new_date"     :pointer (year :uint32) (month  :uint32) (day    :uint32))
(defcfun-export "fbsql_timestamp_new_datetime" :pointer (year :uint32) (month  :uint32) (day    :uint32) (hour :uint32) (minute :uint32) (second :uint32) (tenthousands :uint32))
(defcfun-export "fbsql_timestamp_set_time"     :void    (ts :pointer) (hour :uint32) (minute :uint32) (second :uint32) (tenthousands :uint32))
(defcfun-export "fbsql_timestamp_set_date"     :void    (ts :pointer) (year :uint32) (month  :uint32) (day    :uint32))
(defcfun-export "fbsql_timestamp_set_datetime" :void    (ts :pointer) (year :uint32) (month  :uint32) (day    :uint32) (hour :uint32) (minute :uint32) (second :uint32) (tenthousands :uint32))
(defcfun-export "fbsql_timestamp_get_time"     :void    (ts :pointer) (hour :pointer) (minute :pointer) (second :pointer) (tenthousands :pointer))
(defcfun-export "fbsql_timestamp_get_date"     :void    (ts :pointer) (year :pointer) (month  :pointer) (day    :pointer))
(defcfun-export "fbsql_timestamp_get_datetime" :void    (ts :pointer) (year :pointer) (month  :pointer) (day    :pointer) (hour :pointer) (minute :pointer) (second :pointer) (tenthousands :pointer))
(defcfun-export "fbsql_timestamp_clear"        :void    (ts :pointer))
(defcfun-export "fbsql_timestamp_today"        :void    (ts :pointer))
(defcfun-export "fbsql_timestamp_before"       :uint32     (lhs :pointer) (rhs :pointer))
(defcfun-export "fbsql_timestamp_equal"        :uint32     (lhs :pointer) (rhs :pointer))
(defcfun-export "fbsql_timestamp_now"          :void    (ts :pointer))
(defcfun-export "fbsql_timestamp_delete"       :void    (ts :pointer))

;;;; fbsql_blob functions
(defcfun-export "fbsql_blob_new"       :pointer       (db :pointer) (tr :pointer))
(defcfun-export "fbsql_blob_create"    :void          (blob :pointer))
(defcfun-export "fbsql_blob_cancel"    :void          (blob :pointer))
(defcfun-export "fbsql_blob_open"      :void          (blob :pointer))
(defcfun-export "fbsql_blob_close"     :void          (blob :pointer))
(defcfun-export "fbsql_blob_read"      :uint32           (blob :pointer) (dest :pointer) (size :uint32))
(defcfun-export "fbsql_blob_write"     :void          (blob :pointer) (source :pointer) (size :uint32))
(defcfun-export "fbsql_blob_info"      :void          (blob :pointer) (source :pointer) (size :pointer) (largest :pointer) (segments :pointer))
(defcfun-export "fbsql_blob_save"      :void          (blob :pointer) (data :pointer) (size :uint32))
(defcfun-export "fbsql_blob_size"      :unsigned-int  (blob :pointer))
(defcfun-export "fbsql_blob_load"      :void          (blob :pointer) (data :pointer))
(defcfun-export "fbsql_blob_freemem"   :void          (blob :pointer) (data :pointer))
(defcfun-export "fbsql_blob_delete"    :void          (blob :pointer))

;;;; fbsql_error functions
(defcfun-export "fbsql_error_occured"  :uint32)
(defcfun-export "fbsql_error_msg_size" :unsigned-int)
(defcfun-export "fbsql_error_msg"      :void (buffer :pointer))

(defcenum service-shutdown-modes :force :deny-trans :deny-attach)
(defcenum service-backup-restore-flags (:verbose 1) (:ignore-checksums #x100) (:ignore-limbo #x200) (:metadata-only #x400)
 (:no-garbage-collect #x800) (:non-transportable #x1000) (:convert-ext-tables #x2000) (:replace #x10000)
 (:deactivate-indexes #x20000) (:no-shadow #x40000) (:no-validity #x80000) (:per-table-commit #x100000)
 (:use-all-space #x200000))

(defcfun-export "fbsql_service_new"                    :pointer (server :string) (user :string) (password :string))
(defcfun-export "fbsql_service_connect"                :void    (server :pointer))
(defcfun-export "fbsql_service_connected"              :int32   (server :pointer))
(defcfun-export "fbsql_service_disconnect"             :void    (server :pointer))
(defcfun-export "fbsql_service_get_version_size"       :int32   (server :pointer))
(defcfun-export "fbsql_service_get_version"            :void    (server :pointer) (buffer :pointer))
(defcfun-export "fbsql_service_add_user"               :void    (server :pointer) (user :pointer))
(defcfun-export "fbsql_service_get_user"               :void    (server :pointer) (user :pointer))
(defcfun-export "fbsql_service_modify_user"            :void    (server :pointer) (user :pointer))
(defcfun-export "fbsql_service_remove_user"            :void    (server :pointer) (user :pointer))
(defcfun-export "fbsql_service_set_page_buffers"       :void    (server :pointer) (db :string) (buffers :int32))
(defcfun-export "fbsql_service_set_sweep_interval"     :void    (server :pointer) (db :string) (sweep :int32))
(defcfun-export "fbsql_service_set_sync_write"         :void    (server :pointer) (db :string) (sync :int32))
(defcfun-export "fbsql_service_set_read_only"          :void    (server :pointer) (db :string) (read-only :int32))
(defcfun-export "fbsql_service_set_reserve_space"      :void    (server :pointer) (db :string) (read-only :int32))
(defcfun-export "fbsql_service_shutdown"               :void    (server :pointer) (db :string) (mode service-shutdown-modes) (timeout :int32))
(defcfun-export "fbsql_service_restart"                :void    (server :pointer) (db :string))
(defcfun-export "fbsql_service_sweep"                  :void    (server :pointer) (db :string))
(defcfun-export "fbsql_service_start_backup"           :void    (server :pointer) (db :string) (fbk :string) (flags service-backup-restore-flags))
(defcfun-export "fbsql_service_start_restore"          :void    (server :pointer) (fbk :string) (db :string) (pagesize :int) (flags service-backup-restore-flags))
(defcfun-export "fbsql_service_wait_msg"               :pointer (server :pointer))
(defcfun-export "fbsql_service_wait"                   :void    (server :pointer))
(defcfun-export "fbsql_service_get_user_list"          :pointer (server :pointer))
(defcfun-export "fbsql_service_delete"                 :void    (server :pointer))

(defcfun-export "fbsql_user_new"                    :pointer)
(defcfun-export "fbsql_user_get_name_size"          :int32   (user :pointer))
(defcfun-export "fbsql_user_get_name"               :int32   (user :pointer) (buf :string))
(defcfun-export "fbsql_user_get_password_size"      :int32   (user :pointer))
(defcfun-export "fbsql_user_get_password"           :int32   (user :pointer) (buf :string))
(defcfun-export "fbsql_user_get_firstname_size"     :int32   (user :pointer))
(defcfun-export "fbsql_user_get_firstname"          :int32   (user :pointer) (buf :string))
(defcfun-export "fbsql_user_get_lastname_size"      :int32   (user :pointer))
(defcfun-export "fbsql_user_get_lastname"           :int32   (user :pointer) (buf :string))
(defcfun-export "fbsql_user_get_middlename_size"    :int32   (user :pointer))
(defcfun-export "fbsql_user_get_middlename"         :int32   (user :pointer) (buf :string))
(defcfun-export "fbsql_user_get_userid"             :int32   (user :pointer))
(defcfun-export "fbsql_user_get_groupid"            :int32   (user :pointer))
(defcfun-export "fbsql_user_set_name"               :void    (user :pointer) (name :string))
(defcfun-export "fbsql_user_set_password"           :void    (user :pointer) (password :string))
(defcfun-export "fbsql_user_set_firstname"          :void    (user :pointer) (firstname :string))
(defcfun-export "fbsql_user_set_middlename"         :void    (user :pointer) (middlename :string))
(defcfun-export "fbsql_user_set_lastname"           :void    (user :pointer) (lastname :string))
(defcfun-export "fbsql_user_set_userid"             :void    (user :pointer) (userid :int32))
(defcfun-export "fbsql_user_set_groupid"            :void    (user :pointer) (groupid :int32))
(defcfun-export "fbsql_user_delete"                 :void    (user :pointer))

(defcfun-export "fbsql_user_list_count"             :uint32  (users :pointer))
(defcfun-export "fbsql_user_list_get"               :pointer (users :pointer) (index :uint32))
(defcfun-export "fbsql_user_list_delete"            :void    (users :pointer))