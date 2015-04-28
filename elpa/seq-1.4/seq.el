;;; seq.el --- Sequence manipulation functions  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 1.4
;; Package: seq

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.
;;
;; All functions are tested in test/automated/seq-tests.el

;;; Code:

(defmacro seq-doseq (spec &rest body)
  "Loop over a sequence.
Similar to `dolist' but can be applied lists, strings and vectors.

Evaluate BODY with VAR bound to each element of SEQ, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR SEQ [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (let ((is-list (make-symbol "is-list"))
        (seq (make-symbol "seq"))
        (index (make-symbol "index")))
    `(let* ((,seq ,(cadr spec))
            (,is-list (listp ,seq))
            (,index (if ,is-list ,seq 0)))
       (while (if ,is-list
                  (consp ,index)
                (< ,index (seq-length ,seq)))
         (let ((,(car spec) (if ,is-list
                                (car ,index)
                              (seq-elt ,seq ,index))))
           ,@body
           (setq ,index (if ,is-list
                            (cdr ,index)
                          (+ ,index 1)))))
       ,@(if (cddr spec)
             `((setq ,(car spec) nil) ,@(cddr spec))))))

(defun seq-drop (seq n)
  "Return a subsequence of SEQ without its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, SEQ is returned."
  (if (<= n 0)
      seq
    (if (listp seq)
        (seq--drop-list seq n)
      (let ((length (seq-length seq)))
        (seq-subseq seq (min n length) length)))))

(defun seq-take (seq n)
  "Return a subsequence of SEQ with its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, an empty sequence is
returned."
  (if (listp seq)
      (seq--take-list seq n)
    (seq-subseq seq 0 (min (max n 0) (seq-length seq)))))

(defun seq-drop-while (pred seq)
  "Return a sequence from the first element for which (PRED element) is nil in SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (seq--drop-while-list pred seq)
    (seq-drop seq (seq--count-successive pred seq))))

(defun seq-take-while (pred seq)
  "Return the successive elements for which (PRED element) is non-nil in SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (seq--take-while-list pred seq)
    (seq-take seq (seq--count-successive pred seq))))

(defun seq-filter (pred seq)
  "Return a list of all the elements for which (PRED element) is non-nil in SEQ."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (seq-map (lambda (elt)
                             (if (funcall pred elt)
                                 elt
                               exclude))
                           seq))))

(defun seq-remove (pred seq)
  "Return a list of all the elements for which (PRED element) is nil in SEQ."
  (seq-filter (lambda (elt) (not (funcall pred elt)))
              seq))

(defun seq-reduce (function seq initial-value)
  "Reduce the function FUNCTION across SEQ, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQ, then calling FUNCTION with that result and
the second element of SEQ, then with that result and the third
element of SEQ, etc.

If SEQ is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p seq)
      initial-value
    (let ((acc initial-value))
      (seq-doseq (elt seq)
        (setq acc (funcall function acc elt)))
      acc)))

(defun seq-some-p (pred seq)
  "Return any element for which (PRED element) is non-nil in SEQ, nil otherwise."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (throw 'seq--break elt)))
    nil))

(defun seq-every-p (pred seq)
  "Return non-nil if (PRED element) is non-nil for all elements of the sequence SEQ."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (or (funcall pred elt)
          (throw 'seq--break nil)))
    t))

(defun seq-count (pred seq)
  "Return the number of elements for which (PRED element) is non-nil in SEQ."
  (let ((count 0))
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (setq count (+ 1 count))))
    count))

(defun seq-empty-p (seq)
  "Return non-nil if the sequence SEQ is empty, nil otherwise."
  (if (listp seq)
      (null seq)
    (= 0 (seq-length seq))))

(defun seq-sort (pred seq)
  "Return a sorted sequence comparing using PRED the elements of SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (sort (seq-copy seq) pred)
    (let ((result (seq-sort pred (append seq nil))))
      (seq-into result (type-of seq)))))

(defun seq-contains-p (seq elt &optional testfn)
  "Return the first element in SEQ that equals to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-some-p (lambda (e)
                (funcall (or testfn #'equal) elt e))
              seq))

(defun seq-uniq (seq &optional testfn)
  "Return a list of the elements of SEQ with duplicates removed.
TESTFN is used to compare elements, or `equal' if TESTFN is nil."
  (let ((result '()))
    (seq-doseq (elt seq)
      (unless (seq-contains-p result elt testfn)
        (setq result (cons elt result))))
    (nreverse result)))

(defun seq-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (cond ((or (stringp seq) (vectorp seq)) (substring seq start end))
        ((listp seq)
         (let (len (errtext (format "Bad bounding indices: %s, %s" start end)))
           (and end (< end 0) (setq end (+ end (setq len (seq-length seq)))))
           (if (< start 0) (setq start (+ start (or len (setq len (seq-length seq))))))
           (when (> start 0)
             (setq seq (nthcdr (1- start) seq))
             (or seq (error "%s" errtext))
             (setq seq (cdr seq)))
           (if end
               (let ((res nil))
                 (while (and (>= (setq end (1- end)) start) seq)
                   (push (pop seq) res))
                 (or (= (1+ end) start) (error "%s" errtext))
                 (nreverse res))
             (seq-copy seq))))
        (t (error "Unsupported sequence: %s" seq))))

(defun seq-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the sequences SEQS.
TYPE must be one of following symbols: vector, string or list.

\n(fn TYPE SEQUENCE...)"
  (pcase type
    (`vector (apply #'vconcat seqs))
    (`string (apply #'concat seqs))
    (`list (apply #'append (append seqs '(nil))))
    (t (error "Not a sequence type name: %s" type))))

(defun seq-mapcat (function seq &optional type)
  "Concatenate the result of applying FUNCTION to each element of SEQ.
The result is a sequence of type TYPE, or a list if TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function seq)))

(defun seq-partition (seq n)
  "Return a list of the elements of SEQ grouped into sub-sequences of length N.
The last sequence may contain less than N elements.  If N is a
negative integer or 0, nil is returned."
  (unless (< n 1)
    (let ((result '()))
      (while (not (seq-empty-p seq))
        (push (seq-take seq n) result)
        (setq seq (seq-drop seq n)))
      (nreverse result))))

(defun seq-intersection (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in both SEQ1 and SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains-p seq2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-difference (seq1 seq2 &optional testfn)
  "Return a list of th elements that appear in SEQ1 but not in SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains-p seq2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-group-by (function seq)
  "Apply FUNCTION to each element of SEQ.
Separate the elements of SEQ into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse seq)
   nil))

(defalias 'seq-reverse
  (if (ignore-errors (reverse [1 2]))
      #'reverse
    (lambda (seq)
      "Return the reversed copy of list, vector, or string SEQ.
See also the function `nreverse', which is used more often."
      (let ((result '()))
        (seq-map (lambda (elt) (push elt result))
                 seq)
        (if (listp seq)
            result
          (seq-into result (type-of seq)))))))

(defun seq-into (seq type)
  "Convert the sequence SEQ into a sequence of type TYPE.
TYPE can be one of the following symbols: vector, string or list."
  (pcase type
    (`vector (vconcat seq))
    (`string (concat seq))
    (`list (append seq nil))
    (t (error "Not a sequence type name: %s" type))))

(defun seq--drop-list (list n)
  "Return a list from LIST without its first N elements.
This is an optimization for lists in `seq-drop'."
  (while (and list (> n 0))
    (setq list (cdr list)
          n (1- n)))
  list)

(defun seq--take-list (list n)
  "Return a list from LIST made of its first N elements.
This is an optimization for lists in `seq-take'."
  (let ((result '()))
    (while (and list (> n 0))
      (setq n (1- n))
      (push (pop list) result))
    (nreverse result)))

(defun seq--drop-while-list (pred list)
  "Return a list from the first element for which (PRED element) is nil in LIST.
This is an optimization for lists in `seq-drop-while'."
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(defun seq--take-while-list (pred list)
  "Return the successive elements for which (PRED element) is non-nil in LIST.
This is an optimization for lists in `seq-take-while'."
  (let ((result '()))
    (while (and list (funcall pred (car list)))
      (push (pop list) result))
    (nreverse result)))

(defun seq--count-successive (pred seq)
  "Return the number of successive elements for which (PRED element) is non-nil in SEQ."
  (let ((n 0)
        (len (seq-length seq)))
    (while (and (< n len)
                (funcall pred (seq-elt seq n)))
      (setq n (+ 1 n)))
    n))

(defun seq--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in seq."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '("\\<seq-doseq\\>")))

(defalias 'seq-copy #'copy-sequence)
(defalias 'seq-elt #'elt)
(defalias 'seq-length #'length)
(defalias 'seq-do #'mapc)
(defalias 'seq-each #'seq-do)
(defalias 'seq-map #'mapcar)

(add-to-list 'emacs-lisp-mode-hook #'seq--activate-font-lock-keywords)

;;;; ChangeLog:

;; 2015-04-15  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	seq.el update
;; 
;; 	* packages/seq/seq.el: Update seq.el to version 1.4
;; 	* packages/seq/tests/seq-tests.el: Update seq.el to version 1.4
;; 
;; 2015-03-25  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	Rephrases a comment in seq.el about the order of the arguments
;; 
;; 	* packages/seq/seq.el: Better comment about the order of the arguments
;; 
;; 2015-03-09  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	Update seq.el to version 1.3
;; 
;; 	* packages/seq/seq.el: update to version 1.3
;; 	* packages/seq/tests/seq-tests.el: update to version 1.3
;; 
;; 2015-02-11  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	Update seq.el to version 1.2
;; 
;; 	* package/seq/seq.el: Update to version 1.2
;; 	* packages/seq/tests/seq-tests.el: Update to version 1.2
;; 
;; 2015-02-09  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	Update seq.el to version 1.1.1
;; 
;; 	* package/seq/seq.el: Update to version 1.1.1
;; 	* packages/seq/tests/seq-tests.el: Update to version 1.1.1
;; 
;; 2015-02-06  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	Update seq.el to version 1.1
;; 
;; 	* packages/seq/seq.el: Update to version 1.1
;; 	* packages/seq/tests/seq-tests.el: Update to version 1.1
;; 
;; 2015-01-14  Nicolas Petton  <nicolas@petton.fr>
;; 
;; 	packages/seq: New package
;; 


(provide 'seq)
;;; seq.el ends here
