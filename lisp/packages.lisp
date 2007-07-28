(asdf:oos 'asdf:load-op :cffi)

(defpackage :fbsql-cffi
 (:use :common-lisp :cffi))

(defpackage :fbsql
 (:use :common-lisp :cffi :fbsql-cffi)
 (:export 
   :with-database
   :database-create
   :database-connect
   :database-inactivate
   :database-disconnect
   :database-drop
   :database-get-statistics
   :database-get-counts
   :database-get-users
   :database-get-dialect
   :database-get-servername
   :database-get-database
   :database-get-user
   :database-get-password
   :database-get-role
   :database-get-charset
   :database-get-create-params
   :database-delete
   :with-transaction
   :transaction-start
   :transaction-started
   :transaction-rollback
   :transaction-commit
   :transaction-commit-retaining
   :transaction-add-reservation
   :transaction-attach-database
   :with-statement
   :statement-prepare
   :statement-execute
   :statement-execute-immediate
   :statement-affected-rows
   :statement-get-sql
   :statement-get-plan
   :statement-close
   :statement-fetch
   :statement-column-is-null
   :statement-column-is-null-by-name
   :statement-column-num
   :statement-column-name
   :statement-column-alias
   :statement-column-table
   :statement-column-type
   :statement-column-subtype
   :statement-column-size
   :statement-column-count
   :statement-parameter-type
   :statement-parameter-subtype
   :statement-parameter-size
   :statement-parameter-scale
   :statement-parameter-count
   :statement-get-bool
   :statement-get-short
   :statement-get-integer
   :statement-get-longint
   :statement-get-string
   :statement-get-float
   :statement-get-double
   :statement-get-timestamp
   :statement-set-null
   :statement-set-bool
   :statement-set-short
   :statement-set-integer
   :statement-set-longint
   :statement-set-string
   :statement-set-float
   :statement-set-double
   :statement-set-timestamp
   :statement-delete))

(load "fbsql-cffi.lisp")
