fbsql-bindings for Common Lisp

The lisp-binding of fbsql consists of two layers: The first layer is the raw
CFFI-binding of the C-functions, including the fbsql-prefix.  The second layer
beatifies those function-calls somehow, so they can take and return native
Common Lisp objects and drops the fbsql-prefix, because they already exist
in the fbsql-package.

So instead of fbsql-database-connect you write fbsql:database-connect or
simply database-connect, if your package already uses fbsql. Additionally,
there are three with-macros: with-database, with-transaction and with-statement.
They take a list as it's first argument, followed by the body-forms. The first
item of the list is a variable-name that is bound to the object within the
body-forms. The with-macros handle the underlying C-object's cleanup.
The beatified versions of the fbsql-functions call fbsql-handle-error after
each API-call, which in turn checks for the last error and translates it to a
lisp-condition of type database-error. So when you want to catch error-conditions,
handle you can simply database-error with a handler-case.

More extensive documentation may/will be added later. For now, the best reference
is the C++-API that fbsql is built on top of, ibpp. 
See <http://www.ibpp.org/reference> for a complete class- and function-list.
