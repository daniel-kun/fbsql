(in-package :fbsql-cffi)

(define-foreign-library libfbsql
  (t (:or "libfbsql" "../libfbsql.so" "../libfbsql.dll")))

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

;;; Tests

(define-condition database-error (error)
 ((msg :initarg :error-message :reader error-message)))

(defparameter *fbsql-test-error-occured* nil)
(defparameter *fbsql-log-stream* t)

(export 
 (defun fbsql-handle-error ()
 "If an error has occured one of the last fbsql-functions, translate it to a database-error condition"
 (when (not (eq (fbsql-error-occured) 0))
  (let ((error-size (fbsql-error-msg-size)))
   (with-foreign-pointers
    ((error-msg (+ error-size 1)))
    (fbsql-error-msg error-msg)
    (setf *fbsql-test-error-occured* t)
    (error 'database-error :error-message (concatenate 'string "Exception! " (foreign-string-to-lisp error-msg))))))))

(defun fbsql-print-get (result var field type)
 "Print either result or NULL, if 0 has been returned"
 (if (eq result 0)
  (format t "~a: ~a~%" field (mem-ref var type))
  (format t "~a: NULL~%" field)))

(defmacro fbsql-handle-errors (&body body)
 "Print (if :print-forms has been supplied) and then execute each form in 'body', followed by (fbsql-handle-error). 
 An optional :ignore as the car of a form in 'body' executes the cdr of that form, but does not print it. 
 If :print-forms is the car of a form, all following forms will be printed"
 (let ((print-forms nil))
  `(progn ,@(loop for b in body collect
        (if (eq (first b) :print-forms)
         (setf print-forms t)
         (if (eq (first b) :ignore)
          (fbsql-handle-errors (rest b))
          (if print-forms
           `(progn
              (format t "Testing: ~a~%" ',b)
              ,b
              (fbsql-handle-error))
           `(progn
              ,b
              (fbsql-handle-error)))))))))

(defun fbsql-test-database (database db-user db-pass)
 "Test the fbsql_database binding"
 (let ((db-path (concatenate 'string database "test-database.fdb")))
 (with-foreign-objects 
  ((db :pointer) (users :pointer))
  (with-foreign-pointers ((stat-fetches  4) (stat-marks    4) (stat-reads    4) (stat-writes        4)
                          (count-inserts 4) (count-updates 4) (count-deletes 4) (count-read-indexes 4) 
                          (count-read-sequential 4))
   (fbsql-handle-errors
    (setf db (fbsql-database-new "" db-path db-user db-pass "" "" ""))
    (fbsql-database-create db 1)
    (fbsql-database-inactivate db)
    (fbsql-database-disconnect db)
    (fbsql-database-connect db)
    (fbsql-database-statistics db stat-fetches stat-marks stat-reads stat-writes)
    (:ignore (format t "stat-fetches: ~a~%stat-marks:   ~a~%stat-reads:   ~a~%stat-writes:  ~a~%"
              (mem-ref stat-fetches :uint32) (mem-ref stat-marks  :uint32)
              (mem-ref stat-reads   :uint32) (mem-ref stat-writes :uint32)))
    (fbsql-database-counts db count-inserts count-updates count-deletes count-read-indexes count-read-sequential)
    (:ignore (format t "count-read-sequential: ~a~%count-inserts:         ~a~%count-updates:         ~a~%count-deletes:         ~a~%count-read-indexes:    ~a~%"
              (mem-ref count-read-sequential :uint32) (mem-ref count-inserts :uint32)
              (mem-ref count-updates         :uint32) (mem-ref count-deletes :uint32)
              (mem-ref count-read-indexes    :uint32)))
    (format t "dialect: ~a~%" (fbsql-database-dialect db))
    (fbsql-test-db-str-result fbsql-database-servername    "servername"    servername)
    (fbsql-test-db-str-result fbsql-database-database      "database"      database)
    (fbsql-test-db-str-result fbsql-database-user          "user"          dbuser)
    (fbsql-test-db-str-result fbsql-database-password      "password"      password)
    (fbsql-test-db-str-result fbsql-database-role          "role"          role)
    (fbsql-test-db-str-result fbsql-database-charset       "charset"       charset)
    (fbsql-test-db-str-result fbsql-database-create-params "create-params" create-params)
    (setf users (fbsql-database-users db))
    (:ignore (loop for i from 0 below (fbsql-users-count users) do 
              (format t "User: ~a~%" 
               (fbsql-users-get users 
                (translate-to-foreign i :uint32)))))
    (fbsql-users-delete users)
    (fbsql-database-drop db)
    (fbsql-database-delete db))))))

(defun fbsql-test-transaction (database db-user db-pass)
 "Test the fbsql-transaction binding"
 (let ((db-path (concatenate 'string database "test-transaction.fdb")))
  (with-foreign-objects 
   ((db :pointer) (tr :pointer))
   (fbsql-handle-errors
    (setf db (fbsql-database-new "" db-path db-user db-pass "" "" ""))
    (fbsql-database-create db 1)
    (fbsql-database-connect db)
    (setf tr (fbsql-transaction-new db 
              (or :read :write)
              :concurrency
              :wait
              :auto-commit))
    (fbsql-transaction-start tr)
    (format t "Started? ~a~%" (fbsql-transaction-started tr))
    (fbsql-transaction-commit-retaining tr)
    (fbsql-transaction-commit tr)
    (fbsql-transaction-delete tr)
    (fbsql-database-drop db)
    (fbsql-database-delete db)))))

(defun fbsql-test-loop-results (st)
 "Loop over the results of the executed select-statement used to test each field-type"
 (with-foreign-pointers
  ((field-id     4)
   (field-short  2)
   (field-int    4)
   (field-long   8)
   (field-float  (foreign-type-size :float))
   (field-double (foreign-type-size :double)))
  (loop until (eq (fbsql-statement-fetch st) 0) do
   (fbsql-handle-errors
    (:print-forms)
    (fbsql-print-get (fbsql-statement-get-integer  st 1 field-id    ) field-id     "id"     :uint32)
    (fbsql-print-get (fbsql-statement-get-short    st 2 field-short ) field-short  "short"  :uint16)
    (fbsql-print-get (fbsql-statement-get-integer  st 3 field-int   ) field-int    "int"    :uint32)
    (fbsql-print-get (fbsql-statement-get-longint  st 4 field-long  ) field-long   "long"   :uint64)
    (let ((s-size (fbsql-statement-get-string-size st 5)))
     (with-foreign-pointer (s-buf s-size)
      (if (eq (fbsql-statement-get-string st 5 s-buf) 0)
       (format t "string: ~a~%" (foreign-string-to-lisp s-buf))
       (format t "string: NULL~%"))))
    (fbsql-print-get (fbsql-statement-get-float    st 6 field-float ) field-float  "float"  :float)
    (fbsql-print-get (fbsql-statement-get-double   st 7 field-double) field-double "double" :double)
    (with-foreign-object
     (timestamp :pointer)
     (setf timestamp (fbsql-statement-get-timestamp st 8))
     (if (not (eq timestamp 0))
      (with-foreign-pointers
       ((ts-year         4)
        (ts-month        4)
        (ts-day          4)
        (ts-hour         4)
        (ts-minute       4)
        (ts-second       4)
        (ts-tenthousands 4))
       (fbsql-timestamp-get-datetime timestamp ts-year ts-month ts-day ts-hour ts-minute ts-second ts-tenthousands)
       (format t "Timestamp: ~a.~a.~a ~a:~a:~a:~a~%" 
        (mem-ref ts-day :uint32)
        (mem-ref ts-month :uint32)
        (mem-ref ts-year :uint32)
        (mem-ref ts-hour :uint32)
        (mem-ref ts-minute :uint32)
        (mem-ref ts-second :uint32)
        (mem-ref ts-tenthousands :uint32))
       (fbsql-timestamp-delete timestamp))
      (format t "Timestamp: NULL~%")))
   (with-foreign-object
    (blob :pointer)
    (setf blob (fbsql-statement-get-blob st 9))
    (if (not (eq blob 0))
     (let ((b-size (fbsql-blob-size blob)))
      (with-foreign-pointer
       (b-buf (+ b-size 1))
       (fbsql-blob-load blob b-buf)
       (format t "Blob: ~a~%" (foreign-string-to-lisp b-buf)))
      (fbsql-blob-delete blob))
     (format t "Blob: NULL~%")))))))

(defun fbsql-test-set-params (st db tr)
 "Set the parameters for a test-statement used to test each field-type"
 (fbsql-handle-errors
  (fbsql-statement-set-integer st 1 1)
  (fbsql-statement-set-short   st 2 10)
  (fbsql-statement-set-integer st 3 35000)
  (fbsql-statement-set-longint st 4 64000000000)
  (fbsql-statement-set-string  st 5 "Hallo!")
  (fbsql-statement-set-float   st 6 10.5)
  (fbsql-statement-set-double  st 7 60.1d0)
  (with-foreign-object
   (ts :pointer)
   (setf ts (fbsql-timestamp-new))
   (fbsql-timestamp-set-datetime ts 2007 7 24 15 31 0 0)
   (fbsql-statement-set-timestamp st 8 ts)
   (fbsql-timestamp-delete ts))))

(defparameter test-sql-create-table "create table test(id integer, field_short smallint, field_int integer, field_long bigint, 
   field_string varchar(255), field_float numeric(15,2), field_double numeric(15,2), field_timestamp timestamp, field_blob blob sub_type text,
   constraint pk_test primary key(id))")
(defparameter test-create-generator "create generator test_gen")
(defparameter test-sql-insert "insert into test(field_short,field_int,field_long,field_string,field_float,field_double,field_timestamp,id)values(
  ?,?,?,?,?,?,?,?)")
(defparameter test-sql-select "select id,field_short,field_int,field_long,field_string,field_float,field_double,field_timestamp,field_blob 
    from test order by id")
(defparameter test-select-param "select field_short,field_int,field_long,field_string,field_float,field_double,field_timestamp,id
   from test where field_short=? and field_int=? and field_long=? and field_string=? and field_float=?
   and field_double=? and field_timestamp=? and id=?")

(defun fbsql-test-statement (database db-user db-pass)
 "Test the fbsql-statement binding, includes testing of error-handling, timestamps and blobs"
 (let ((db-path (concatenate 'string database "test-statement.fdb")))
 (with-foreign-strings
  ((sql-create-table test-sql-create-table)
   (sql-create-generator test-create-generator)
   (sql-insert test-sql-insert)
   (sql-select test-sql-select)
   (sql-select-param test-select-param))
 (with-foreign-objects
  ((db :pointer)
   (tr :pointer)
   (st :pointer))
  (fbsql-handle-errors
   (setf db (fbsql-database-new "" db-path db-user db-pass "" "" ""))
   (fbsql-database-create db 3)
   (fbsql-database-connect db)
   (setf tr (fbsql-transaction-new-with-defaults db))
   (fbsql-transaction-start tr)
   (setf st (fbsql-statement-new db tr))
   (fbsql-statement-prepare st sql-create-table)
   (fbsql-statement-execute st)
   (fbsql-statement-execute-immediate st sql-create-generator)
   (fbsql-transaction-commit-retaining tr)
   (fbsql-statement-prepare st sql-insert)
   (fbsql-statement-execute st)
   (fbsql-transaction-commit-retaining tr)
   (fbsql-statement-prepare st sql-select)
   (fbsql-statement-execute st)
   (fbsql-test-loop-results st)
   (fbsql-transaction-commit-retaining tr)
   (fbsql-statement-prepare st sql-select-param)
   (fbsql-test-set-params st db tr)
   (fbsql-statement-execute st)
   (fbsql-test-loop-results st)
   (fbsql-transaction-commit tr)
   (fbsql-transaction-delete tr)
   (fbsql-database-drop db)
   (fbsql-database-delete db))))))

(export 
 (defun fbsql-test (database db-user db-pass)
  "Test all fbsql_*-bindings"
  (handler-case
   (progn
    (fbsql-test-database database db-user db-pass)
    (fbsql-test-transaction database db-user db-pass)
    (fbsql-test-statement database db-user db-pass))
   (database-error (dberr) (format t "Error! ~a~%" (error-message dberr))))))

(defun fbsql-test-get-string-buffer (size-fun buf-fun user)
 (with-foreign-object
  (size :int32)
  (test-forms
  (setf size (+ (funcall size-fun user) 1))
  (if (eq size 0)
   nil
   (test-forms
   (with-foreign-pointer
    (buffer size)
    (funcall buf-fun user buffer)
    (foreign-string-to-lisp buffer)))))))

(defun fbsql-test-print-user (u)
 (format t "Name: ~a~%
Password: ~a
Firstname: ~a
Middlename: ~a
Lastname: ~a
User-ID: ~a
Group-ID: ~a~%" 
   (fbsql-test-get-string-buffer #'fbsql-user-get-name-size #'fbsql-user-get-name u)
   (fbsql-test-get-string-buffer #'fbsql-user-get-name-size #'fbsql-user-get-password u)
   (fbsql-test-get-string-buffer #'fbsql-user-get-name-size #'fbsql-user-get-firstname u)
   (fbsql-test-get-string-buffer #'fbsql-user-get-name-size #'fbsql-user-get-middlename u)
   (fbsql-test-get-string-buffer #'fbsql-user-get-name-size #'fbsql-user-get-lastname u)
   (fbsql-user-get-userid u)
   (fbsql-user-get-groupid u)))

(defun fbsql-test-print-users (service)
 (with-foreign-objects
         ((users :pointer)
          (user-count :uint32))
         (test-forms
          (format t "~%Service: ~a~%" service)
         (setf users (fbsql-service-get-user-list service))
         (setf user-count (fbsql-user-list-count users))
         (loop for i from 0 below user-count do
          (fbsql-test-print-user (fbsql-user-list-get users i))))))

(defun fbsql-test-output-progress (service)
 (with-foreign-object
  (gbak-output :pointer)
  (loop while (not (or (eq (setf gbak-output (fbsql-service-wait-msg service)) 0) (null gbak-output))) do 
   (format t "~a~%" (foreign-string-to-lisp gbak-output)))))

(defun fbsql-test-build-fbk (db-path)
 (format nil "~a~a" db-path ".fbk"))

(defun fbsql-test-service (server db-path db-user db-pass)
 (handler-case
  (with-foreign-object
   (s :pointer)
   (setf s (fbsql-service-new server db-user db-pass))
   (format t "Service: ~a~%" s)
   (if (not (eq s 0))
    (fbsql-handle-errors
     (test-forms
      (fbsql-service-connect s)
      (if (eq (fbsql-service-connected s) 0)
       (error "Service-connection failed")
       (test-forms
        (fbsql-service-disconnect s)
        (fbsql-service-connect s)
        (with-foreign-object
         (v-size :int32)
         (setf v-size (fbsql-service-get-version-size s))
         (if (> v-size 0)
          (with-foreign-pointer
           (version v-size)
           (fbsql-service-get-version s version)
           (format t "Version: ~a~%" version)
           (format t "Version: ~a~%" (foreign-string-to-lisp version)))
          (format t "Could not get version info.")))
        (fbsql-test-print-users s)
        (fbsql-service-start-backup s db-path (fbsql-test-build-fbk db-path) :verbose)
        (fbsql-test-output-progress s)
        (fbsql-service-start-restore s (fbsql-test-build-fbk db-path) db-path 1024 (cenum-or 'service-backup-restore-flags :verbose :replace))
        (fbsql-test-output-progress s)))))))
  (database-error (dberr) (format t "Error! ~a~%" (error-message dberr)))))
