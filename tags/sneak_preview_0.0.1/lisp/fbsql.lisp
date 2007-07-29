;;;; Copyright 2007 Daniel Albuschat
;;;; See license.txt for further license information
;;;;
;;;; This package has been written by a lisp-beginner, so please bear with anything you find strange.
;;;; Feel free to contact d.albuschat@gmail.com for any comments or suggestions.

(in-package :fbsql)

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

(define-checked-fun database-new    "Create a firebird-database object" fbsql-database-new (server database username password role charset create-params))
(define-checked-fun database-create "Create a firebird-database" fbsql-database-create (db dialect))
(define-checked-fun database-connect "Connect to a firebird-database" fbsql-database-connect (db))
(define-checked-fun database-inactivate "Rollback any uncommited transaction on this database" fbsql-database-inactivate (db))
(define-checked-fun database-disconnect "Disconnect from database" fbsql-database-disconnect (db))
(define-checked-fun database-drop "Drop the database currently connected to" fbsql-database-drop (db))
(defun database-get-statistics (db)
 "Get the database-statistics in a plist. Returned properties are :fetches, :marks, :reads and :writes"
 (fbsql-cffi::with-foreign-pointers
  ((fetches 4) (marks 4) (reads 4) (writes 4))
  (fbsql-database-statistics db fetches marks reads writes)
  (fbsql-handle-error)
  (list :fetches (mem-ref fetches :uint32) :marks (mem-ref marks :uint32) :reads (mem-ref reads :uint32) :writes (mem-ref writes :uint32))))

(defun database-get-counts (db)
 "Get the database-access-counts in a plist. Returned properties are :inserts, :updates, :deletes, :read-indexes and :read-sequential"
 (fbsql-cffi::with-foreign-pointers
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

(defun test-database (dir db-user db-pass)
 "Tests all database-functions"
 (handler-bind
  ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'drop-database))))
  (with-database 
   (db "localhost" (concatenate 'string dir "test-database.fdb") db-user db-pass "" "iso8859_1" "")
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

(defun test-transaction (db-path db-user db-pass)
 (with-database
  (db "" db-path db-user db-pass "" "iso8859_1" "")
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
 (fbsql-cffi::with-foreign-pointers
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

(defun test-statement (db-path db-user db-pass)
 (handler-bind
  ((database-error #'(lambda (dberr) (format t "Error! ~a~%" (error-message dberr)) (invoke-restart 'drop-database))))
  (with-database
   (db "" db-path db-user db-pass "" "iso8859_1" "")
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

(defun test (dir db-user db-pass)
 (handler-case
  (progn
   (test-database    dir db-user db-pass)
   (test-transaction dir db-user db-pass)
   (test-statement   dir db-user db-pass))
  (database-error (dberr) (format t "Test failed!~%Error: ~a" (error-message dberr)))))
