;;;; CFFI-bindings for fbsql, the simple Firebird API

(defpackage #:fbsql-asd
 (:use :cl :common-lisp :cffi :asdf))

(in-package :fbsql-asd)

(defsystem fbsql
 :name "fbsql"
 :version "0.1.0"
 :maintainer "Daniel Albuschat"
 :author "Daniel Albuschat"
 :license "BSD"
 :description "fbsql-bindings"
 :long-description "CFFI-bindings for fbsql, the simple firebird API"
 
 :serial t
 :components ((:file "fbsql")))
