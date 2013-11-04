(defvar *default-db-name* (expand-file-name "~/.xemacs/lisp-file-database")
  "Default location of the database")

(defun build-lisp-file-db (&optional db-name path rebuild)
  "Create a database of all lisp files in the directories given by PATH.
DB-NAME is the database name, defaulting to *default-db-name*
PATH is a list of directories to search, defaulting to load-path.
REBUILD "
  (let ((path (or path load-path))
	(db (open-database (or db-name *default-db-name*) nil nil "rw+")))
    ;; For each entry in path, find all files in it and put them in
    ;; the database.
    (dolist (dir path)
      (dolist (file (directory-files dir t nil t t))
	;; Separate the file name and the directory.  The key is the
	;; filename, and the value is the whole pathname.  However, if
	;; the key already exists, DON'T put that entry in.  We want
	;; things that occur first in load-path to override entries
	;; later in load-path
	(let ((fname (file-name-nondirectory file)))
	  (put-database fname file db nil))))))

(defun show-lisp-db (&optional db-name)
  (let ((db (open-database (or db-name *default-db-name*) nil nil "r"))
	(entries '()))
    (map-database #'(lambda (key val)
		      (push (cons key val) entries))
		  db)
    (nreverse entries)))

(defun lookup-lisp-file-db (file &optional db-name)
  (let ((name (file-name-nondirectory file))
	(db (open-database (or db-name *default-db-name*) nil nil "r")))
    (do* ((ext '("" ".elc" ".el") (rest ext))
	 (entry (get-database (concat name (first ext)) db)
		(get-database (concat name (first ext)) db)))
	((or entry (null ext)) entry)
      ())))
    
