;;;; Copyright 2007 Daniel Albuschat
;;;; See license.txt for further license information
;;;;
;;;; This package has been written by a lisp-beginner, so please bear with anything you find strange.
;;;; Feel free to contact d.albuschat@gmail.com for any comments or suggestions.

(in-package :fbsql)


(define-condition database-error (error)
 ((msg :initarg :error-message :reader error-message)))

(defparameter *fbsql-test-error-occured* nil)
(defparameter *fbsql-log-stream* t)

(defun fbsql-handle-error ()
 "If an error has occured one of the last fbsql-functions, translate it to a database-error condition"
 (when (not (eq (fbsql-error-occured) 0))
  (let ((error-size (fbsql-error-msg-size)))
   (with-foreign-pointers
    ((error-msg (+ error-size 1)))
    (fbsql-error-msg error-msg)
    (setf *fbsql-test-error-occured* t)
    (error 'database-error :error-message (concatenate 'string "Exception! " (foreign-string-to-lisp error-msg)))))))

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

(defun fbsql-test-database (database)
 "Test the fbsql_database binding"
 (let ((db-path (concatenate 'string database "test-database.fdb")))
 (with-foreign-objects 
  ((db :pointer) (users :pointer))
  (with-foreign-pointers ((stat-fetches  4) (stat-marks    4) (stat-reads    4) (stat-writes        4)
                          (count-inserts 4) (count-updates 4) (count-deletes 4) (count-read-indexes 4) 
                          (count-read-sequential 4))
   (fbsql-handle-errors
    (setf db (fbsql-database-new "" db-path "sysdba" "5735" "" "" ""))
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

(defun fbsql-test-transaction (database)
 "Test the fbsql-transaction binding"
 (let ((db-path (concatenate 'string database "test-transaction.fdb")))
  (with-foreign-objects 
   ((db :pointer) (tr :pointer))
   (fbsql-handle-errors
    (setf db (fbsql-database-new "" db-path "sysdba" "5735" "" "" ""))
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

(defun fbsql-test-statement (database)
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
   (setf db (fbsql-database-new "" db-path "sysdba" "5735" "" "" ""))
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

(defun test (database)
 "Test all fbsql_*-bindings"
 (fbsql-test-database database)
 (fbsql-test-transaction database)
 (fbsql-test-statement database)
 (if *fbsql-test-error-occured*
  (format t "Test failed ;-(")
  (format t "Test successful!")))

;(test "/pub/firebird/")

(defmacro define-checked-fun (name docs fbsql-name param &optional converter-result converter-param)
 "This macro defines a function with name 'name' that calls the cffi-fbsql-function 'fbsql-name' with params 'param'
 and optionally converts the result with function 'converter'"
 `(defun ,name ,param
    ,docs
    (let ((result (,fbsql-name ,@(loop for p in param collect (if (not (null converter-param)) `(converter-param ,p) p)))))
     (fbsql-handle-error)
     ,(if (null converter-result)
        `result
        `(,converter-result result)))))

(define-checked-fun database-create "Create a firebird-database" fbsql-database-create (db dialect))
(define-checked-fun database-connect "Connect to a firebird-database" fbsql-database-connect (db))
(define-checked-fun database-inactivate "Rollback any uncommited transaction on this database" fbsql-database-inactivate (db))
(define-checked-fun database-disconnect "Disconnect from database" fbsql-database-disconnect (db))
(define-checked-fun database-drop "Drop the database currently connected to" fbsql-database-drop (db))
(defun database-get-statistics (db)
 "Get the database-statistics in a plist. Returned properties are :fetches, :marks, :reads and :writes"
 (with-foreign-pointers
  ((fetches 4) (marks 4) (reads 4) (writes 4))
  (fbsql-database-statistics db fetches marks reads writes)
  (fbsql-handle-error)
  (list :fetches (mem-ref fetches :uint32) :marks (mem-ref marks :uint32) :reads (mem-ref reads :uint32) :writes (mem-ref writes :uint32))))

(defun database-get-counts (db)
 "Get the database-access-counts in a plist. Returned properties are :inserts, :updates, :deletes, :read-indexes and :read-sequential"
 (with-foreign-pointers
  ((inserts 4) (updates 4) (deletes 4) (read-indexes 4) (read-sequential 4))
  (fbsql-database-counts db inserts updates deletes read-indexes read-sequential)
  (fbsql-handle-error)
  (list 
   :inserts         (mem-ref inserts         :uint32)
   :updates         (mem-ref updates         :uint32)
   :deletes         (mem-ref deletes         :uint32)
   :read-indexes    (mem-ref read-indexes    :uint32)
   :read-sequential (mem-ref read-sequential :uint32))))

(defun database-get-users (db)
 "Returns all connected users in a list"
 (let (users-list)
  (with-foreign-object
   (users :pointer)
   (setf users (fbsql-database-users db))
   (if (or (null users) (eq users 0))
    (fbsql-handle-error)
    (let ((users-count (fbsql-users-count users)))
     (fbsql-handle-error)
     (loop for i from 0 below users-count do
      (progn 
       (push (fbsql-users-get users (translate-to-foreign i :uint32)) users-list) 
       (fbsql-handle-error))))))
  users-list))
(define-checked-fun database-get-dialect       "Returns the database's dialect" fbsql-database-dialect (db))
(define-checked-fun database-get-servername    "Returns the server that has been connected to"                 
 fbsql-database-servername    (db) foreign-string-to-lisp)
(define-checked-fun database-get-database      "Returns the database currently connected to"                   
 fbsql-database-database      (db) foreign-string-to-lisp)
(define-checked-fun database-get-user          "Returns the user-name currently connected as"                  
 fbsql-database-user          (db) foreign-string-to-lisp)
(define-checked-fun database-get-password      "Returns the user-name currently connected with"                
 fbsql-database-password      (db) foreign-string-to-lisp)
(define-checked-fun database-get-role          "Returns the role currently connected as"                       
 fbsql-database-role          (db) foreign-string-to-lisp)
(define-checked-fun database-get-charset       "Returns the character-set currently connected with"            
 fbsql-database-charset       (db) foreign-string-to-lisp)
(define-checked-fun database-get-create-params "Returns the create-parameters used to connect to the database" 
 fbsql-database-create-params (db) foreign-string-to-lisp)
(define-checked-fun database-delete "Delete the database-object (and disconnects, if connected)" 
 fbsql-database-delete (db))

(defmacro with-database (var-and-params &body body)
 "The first parameter is a form with the following meaning: The first element is the variable-name
 the database-connection will be bound to. The rest (7, all required) are parameters applied to fbsql-database-new,
 in the following order: server, database-file, user, password, role, charset, create-parameters.
 There are two possible restarts: delete-object, which simply delete the connection-object (which implicitly
 rolls all transactions back) and drop-database, which drops the database"
 (let ((varname (first var-and-params))
       (params  (rest var-and-params)))
  `(let ((,varname (fbsql-database-new ,@params)))
      (unwind-protect
       (progn ,@(loop for b in body collect b))
       (fbsql-database-delete ,varname)))))

(defun test-database (dir)
 "Tests all database-functions"
 (handler-bind
  ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'drop-database))))
  (with-database 
   (db "localhost" (concatenate 'string dir "test-database.fdb") "sysdba" "5735" "" "iso8859_1" "")
   (test-forms
    (database-create db 3)
    (database-inactivate db)
    (database-disconnect db)
    (database-connect db)
    (format t "Statistics: ~a~%" (database-get-statistics db))
    (format t "Counts: ~a~%"     (database-get-counts db))
    (let ((users (database-get-users db)))
     (loop for u in users do (format t "User: ~a~%" u)))
    (format t "Dialect: ~a~%" (database-get-dialect db))
    (format t "Servername: ~a~%" (database-get-servername db))
    (format t "Database: ~a~%" (database-get-database db))
    (format t "Username: ~a~%" (database-get-user db))
    (format t "Password: ~a~%" (database-get-password db))
    (format t "Role: ~a~%" (database-get-role db))
    (format t "Charset ~a~%" (database-get-charset db))
    (format t "Create-Parameter: ~a~%" (database-get-create-params db))
    (database-drop db)
    (database-disconnect db)))))

;(test-database "/pub/firebird/")

(defmacro with-transaction (var-and-params defaults &body body)
 (let ((varname (first var-and-params))
       (params  (rest var-and-params)))
  `(let ((,varname ,(if defaults `(fbsql-transaction-new-with-defaults ,@params) `(fbsql-transaction-new ,@params))))
     (unwind-protect
       (progn ,@(loop for b in body collect b))
       (fbsql-transaction-delete ,varname)))))

(defun helper-translate-bool-to-foreign (b)
 (if b 1 0))
(defun helper-translate-foreign-to-bool (b)
 (not (eq b 0)))
(define-checked-fun transaction-start "Start a new transaction" fbsql-transaction-start (tr))
(define-checked-fun transaction-started "Is the transaction running?" fbsql-transaction-started (tr) helper-transalte-foreign-bool)
(define-checked-fun transaction-rollback "Rollback and close the transaction" fbsql-transaction-rollback (tr))
(define-checked-fun transaction-commit "Commit and close the transaction" fbsql-transaction-commit (tr))
(define-checked-fun transaction-commit-retaining "Commit the transaction, but keep it alive" fbsql-transaction-commit-retaining (tr))
(defun transaction-add-reservation (tr db table &rest rest)
 (fbsql-transaction-add-reservation tr db table (apply #'cenum-or (cons 'fbsql-transaction-table-reservation rest)))
 (fbsql-handle-error))
(defun transaction-attach-database (tr db access-mode isolation-level lock-resolution factory-flags)
 (fbsql-transaction-attach-database tr db 
  (apply #'cenum-or access-mode)
  (apply #'cenum-or isolation-level)
  (apply #'cenum-or lock-resolution)
  (apply #'cenum-or factory-flags)))
(define-checked-fun transaction-detach-database "Detach from given database" fbsql-transaction-detach-database (tr db))
(define-checked-fun transaction-delete "Delete transaction-object" fbsql-transaction-delete (tr))

(defun test-transaction (db-path)
 (with-database
  (db "" db-path "sysdba" "5735" "" "iso8859_1" "")
  (handler-bind
   ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'rollback-transaction))))
   (database-connect db)
   (with-transaction (tr db) t
    (test-forms
     (transaction-add-reservation tr db "test" :shared-write :protected-read)
     (transaction-detach-database tr db)
     (transaction-attach-database tr db '(:read) '(:read-commited) '(:wait) '(:ignore-limbo))
     (transaction-start tr)
     (transaction-commit-retaining tr)
     (transaction-commit tr)
     (transaction-start tr)
     (transaction-rollback tr)
     (transaction-detach-database tr db))))))

;(test-transaction "/pub/firebird/Test1.fdb")

(defmacro with-statement (var-and-params &body body)
 (let ((varname (first var-and-params))
       (params  (rest var-and-params)))
  `(let ((,varname (fbsql-statement-new ,@params)))
     (unwind-protect
      (progn 
       ,@(loop for b in body collect b))
       (fbsql-statement-delete ,varname)))))

(define-checked-fun statement-prepare                    "Prepare a statement"               
 fbsql-statement-prepare           (st sql))
(define-checked-fun statement-execute                    "Execute a prepared statement"      
 fbsql-statement-execute           (st))
(define-checked-fun statement-execute-immediate          "Prepare and execute the statement" 
 fbsql-statement-execute-immediate (st sql))
(define-checked-fun statement-affected-rows              "Return the 'affected rows' of the statement. Read firebird-docs on the meaning of this value" 
 fbsql-statement-execute-immediate (st sql))
(define-checked-fun statement-get-sql                    "Get the prepared sql-statement"
 fbsql-statement-sql               (st))
(define-checked-fun statement-get-plan                   "Return the plan of the prepared sql-statement"
 fbsql-statement-plan              (st))
(define-checked-fun statement-close                      "Close an executed statement"
 fbsql-statement-close             (st))
(define-checked-fun statement-fetch                      "Fetch the results of an executed select-statement"
 fbsql-statement-fetch             (st) helper-translate-foreign-to-bool)
(define-checked-fun statement-column-is-null             "Is the fetched column (by index) NULL?"
 fbsql-statement-is-null           (st column))
(define-checked-fun statement-column-is-null-by-name     "Is the fetched column (by name) NULL?"
 fbsql-statement-is-null           (st column))
(define-checked-fun statement-column-num                 "Return the index of the named column"
 fbsql-statement-column-num        (st column))
(define-checked-fun statement-column-name                "Return the name of the nth column"
 fbsql-statement-column-name       (st column))
(define-checked-fun statement-column-alias               "Return the alias of the nth column"
 fbsql-statement-column-alias      (st column))
(define-checked-fun statement-column-table               "Return the table the nth column belongs to"
 fbsql-statement-column-table      (st column))
(define-checked-fun statement-column-type                "Return the type of the nth column"
 fbsql-statement-column-type       (st column))
(define-checked-fun statement-column-subtype             "Return the subtype of the nth column"
 fbsql-statement-column-subtype    (st column))
(define-checked-fun statement-column-size                "Return the size of the nth column"
 fbsql-statement-column-size       (st column))
(define-checked-fun statement-column-count               "Return number of the available columns"
 fbsql-statement-column-count      (st column))
(define-checked-fun statement-parameter-type             "Return the type of the nth parameter"
 fbsql-statement-parameter-type    (st parameter))
(define-checked-fun statement-parameter-subtype          "Return the subtype of the nth parameter"
 fbsql-statement-parameter-subtype (st parameter))
(define-checked-fun statement-parameter-size             "Return the size of the nth parameter"
 fbsql-statement-parameter-size    (st parameter))
(define-checked-fun statement-parameter-scale            "Return the scale of the nth parameter"
 fbsql-statement-parameter-scale   (st parameter))
(define-checked-fun statement-parameter-count            "Return number of the available parameters"
 fbsql-statement-parameter-count   (st parameter))

(defmacro define-checked-get-fun (name type docs fbsql-name param &optional converter-result converter-param)
 "This macro defines a function with name 'name' that calls the cffi-fbsql-function 'fbsql-name' with params 'param'
 and optionally converts the result with function 'converter'"
 `(defun ,name ,param
    ,docs
    (with-foreign-pointer
     (ptr (foreign-type-size ,type))
     (let ((result (,fbsql-name ,@(loop for p in param collect (if (not (null converter-param)) `(converter-param ,p) p)) ptr)))
      (fbsql-handle-error)
      (if (not (eq result 0))
       nil
       (progn
        (setf result (mem-ref ptr ,type))
        ,(if (null converter-result)
           `result
           `(,converter-result result))))))))

(define-checked-get-fun statement-get-bool   :uint32      "Return the value of the nth field"
 fbsql-statement-get-bool          (st column) helper-transalte-foreign-to-bool)
(define-checked-get-fun statement-get-short  :uint16      "Return the value of the nth field"
 fbsql-statement-get-short         (st column))
(define-checked-get-fun statement-get-integer :uint32     "Return the value of the nth field"
 fbsql-statement-get-integer       (st column))
(define-checked-get-fun statement-get-longint :uint64     "Return the value of the nth field"
 fbsql-statement-get-longint       (st column))
(defun statement-get-string        (st column)
 "Return the value of the nth field"
 (let ((s-size (fbsql-statement-get-string-size st column)))
  (fbsql-handle-error)
  (if (eq s-size 0)
   ""
   (with-foreign-pointer
    (s-buf s-size)
    (fbsql-statement-get-string st column s-buf)
    (fbsql-handle-error)
    (foreign-string-to-lisp s-buf)))))
(define-checked-get-fun statement-get-float   :float     "Return the value of the nth field"
 fbsql-statement-get-float         (st column))
(define-checked-get-fun statement-get-double  :double    "Return the value of the nth field"
 fbsql-statement-get-double        (st column))

(defun statement-get-timestamp     (st column)
 "Return the value of the nth field"
 (with-foreign-pointers
  ((year 4) (month 4) (day 4) (hour 4) (minute 4) (second 4) (tenthousandths 4))
  (with-foreign-object
   (timestamp :pointer)
   (setf timestamp (fbsql-statement-get-timestamp st column))
   (if (eq timestamp 0)
    nil
    (progn 
     (fbsql-handle-error)
     (fbsql-timestamp-get-datetime timestamp year month day hour minute second tenthousandths)
     (fbsql-handle-error)
     (let ((result (encode-universal-time 
                    (mem-ref second         :int32)
                    (mem-ref minute         :int32)
                    (mem-ref hour           :int32)
                    (mem-ref day            :int32)
                    (mem-ref month          :int32)
                    (mem-ref year           :int32))))
      (fbsql-timestamp-delete timestamp)
      result))))))

;(defcfun "fbsql_statement_get_dbkey"                     :pointer          (st :pointer) (column :uint32))
;(defcfun "fbsql_statement_get_blob"                      :pointer          (st :pointer) (column :uint32))

(define-checked-fun statement-set-null                   "Set the nth parameter to NULL"
 fbsql-statement-set-null           (st column value))
(define-checked-fun statement-set-bool                   "Set the nth parameter"
 fbsql-statement-set-bool           (st column value) nil helper-translate-bool-to-foreign)
(define-checked-fun statement-set-short                  "Set the nth parameter"
 fbsql-statement-set-short          (st column value))
(define-checked-fun statement-set-integer                "Set the nth parameter"
 fbsql-statement-set-integer        (st column value))
(define-checked-fun statement-set-longint                "Set the nth parameter"
 fbsql-statement-set-longint        (st column value))
(define-checked-fun statement-set-string                 "Set the nth parameter"
 fbsql-statement-set-string         (st column value))
(define-checked-fun statement-set-float                  "Set the nth parameter"
 fbsql-statement-set-float          (st column value))
(define-checked-fun statement-set-double                 "Set the nth parameter"
 fbsql-statement-set-double         (st column value))
(defun statement-set-timestamp      (st column timestamp)
 (with-foreign-objects
  ((year :uint32) (month :uint32) (day :uint32) (hour :uint32) (minute :uint32) (sec :uint32) (tenthousandths :uint32))
  (let ((utime (multiple-value-list (decode-universal-time timestamp))))
   (setf sec (first utime) minute (second utime) hour (third utime) day (fourth utime) month (fifth utime) year (sixth utime))
   (with-foreign-object
    (ts :pointer)
     (setf ts (fbsql-timestamp-new))
     (fbsql-timestamp-set-datetime ts year month day hour minute sec 0)
     (fbsql-statement-set-timestamp st column ts)
     (fbsql-timestamp-delete ts)
     (fbsql-handle-error)))))
;(defcfun "fbsql_statement_set_dbkey"                     :void             (st :pointer) (column :uint32) (value :pointer))
;(defcfun "fbsql_statement_set_blob"                      :void             (st :pointer) (column :uint32) (value :pointer))
(define-checked-fun statement-delete                     "Delete the statement object"
 fbsql-statement-delete         (st column value))

(defun gen-id (generator count db tr)
 (with-statement
  (st db tr)
  (statement-prepare st (format nil "select first(1)gen_id(~a,~a) from rdb$database" generator count))
  (statement-execute st)
  (if (statement-fetch st)
   (statement-get-integer st 1)
   (error 'database-error :error-message (format nil "Error while generating id with generator ~a." generator)))))

(defmacro while-statement-fetch (st &body body)
 `(loop until (not (statement-fetch st)) do ,@body))

(defun test-statement-set-params (st db tr)
 "Set the parameters for a test-statement used to test each field-type"
 (format t "~%")
 (test-forms
  (statement-set-short   st 1 10)
  (statement-set-integer st 2 35000)
  (statement-set-longint st 3 64000000000)
  (statement-set-string  st 4 "Hallo!")
  (statement-set-float   st 5 10.5)
  (statement-set-double  st 6 60.1d0)
  (statement-set-timestamp st 7 (get-universal-time))))

(defun test-statement-get-columns (st db tr)
 "Set the parameters for a test-statement used to test each field-type"
 (format t "~%")
 (test-forms
  (format t "result:
short:~t~a
integer:~t~a
longint:~t~a
string:~t~a
float:~t~a
double:~t~a
timestamp:~t~a~%"
  (statement-get-short   st 1)
  (statement-get-integer st 2)
  (statement-get-longint st 3)
  (statement-get-string  st 4)
  (statement-get-float   st 5)
  (statement-get-double  st 6)
  (statement-get-timestamp st 7))))

(defun test-statement (db-path)
 (handler-bind
  ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'drop-database))))
  (with-database
   (db "" db-path "sysdba" "5735" "" "iso8859_1" "")
   (fbsql-database-connect db)
   (fbsql-database-drop db)
   (database-create db 3)
   (database-connect db)
   (handler-bind
    ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'rollback-transaction))))
    (with-transaction (tr db) t
     (transaction-start tr)
     (with-statement (st db tr)
      (test-forms
       (statement-prepare st test-sql-create-table)
       (statement-execute st)
       (transaction-commit-retaining tr)
       (statement-execute-immediate st test-create-generator)
       (transaction-commit-retaining tr)
       (statement-prepare st test-sql-insert)
       (test-statement-set-params st db tr)
       (let ((id (gen-id "test_gen" 1 db tr)))
        (test-forms
        (statement-set-integer st 8 id)
        (statement-execute st)
        (transaction-commit-retaining tr)
        (statement-prepare st test-select-param)
        (test-statement-set-params st db tr)
        (statement-set-integer st 8 id)
        (statement-execute st)))
       (while-statement-fetch st
        (test-statement-get-columns st db tr))
       (transaction-rollback tr))))
    (database-drop db)))))

;(test-statement "/pub/firebird/test-statement.fdb")

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

(defun fbsql-test-service (server db-path)
 (handler-case
  (with-foreign-object
   (s :pointer)
   (setf s (fbsql-service-new server "sysdba" "5735"))
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

(fbsql-test-service "localhost" "/pub/firebird/test-service.fdb")
