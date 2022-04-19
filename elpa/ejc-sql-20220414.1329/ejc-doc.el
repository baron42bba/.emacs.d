;;; ejc-doc.el -- SQL quick hints documentation (the part of ejc-sql).

;;; Copyright © 2018-2019 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Code:

(require 'dash)

(defconst ejc-sql-doc (make-hash-table :test 'eq))

(defun ejc-fill-doc (hash-const &rest args)
  (-reduce (lambda (key val)
             (puthash key val hash-const))
           args))

(defvar ejc-doc-created-p nil)

(defun ejc-create-doc ()
  (ejc-fill-doc
   ejc-sql-doc
   'select
   "The SQL SELECT statement returns a result
set of records from one or more tables.

SELECT
       [DISTINCT | DISTINCTROW | ALL]
       select_expression,...
   FROM table_references
     [WHERE where_definition]
     [GROUP BY {unsigned_integer | col_name | formula}]
     [HAVING where_definition]
     [ORDER BY {unsigned_integer | col_name | formula} [ASC | DESC], ...]"

   'insert
   "An SQL INSERT statement adds one or more records
to any single table in a relational database.

INSERT INTO table (column1 [, column2, column3 ... ])
VALUES (value1 [, value2, value3 ... ])"

   'update
   "An SQL UPDATE statement changes the data of one
or more records in a table. Either all the rows can
be updated, or a subset may be chosen using a condition.

UPDATE table_name SET column_name = value [, column_name = value ...]
[WHERE condition]"

   'delete
   "The SQL DELETE statement removes one or more records from a table.

DELETE FROM table_name [WHERE condition]"

   'alter
   "The ALTER TABLE command modifies column definitions and table constraints
'on the fly'. This means existing definitions are extended, changed or
deleted or existing data is casted to a different type or existing data is
evaluated against the new definitions.

-- change column definitions
ALTER TABLE <table_name> { ADD | ALTER } [ COLUMN ]
            <column_name> <column_definition>;
ALTER TABLE <table_name> { DROP        } [ COLUMN ]
            <column_name>;

-- change table constraints
ALTER TABLE <table_name> { ADD | ALTER } CONSTRAINT
            <constraint_name> <constraint_definition>;
ALTER TABLE <table_name> { DROP        } CONSTRAINT
            <constraint_name>;"
   'distinct
   "Return different values")
 (setq ejc-doc-created-p t))

(provide 'ejc-doc)

;;; ejc-doc.el ends here
