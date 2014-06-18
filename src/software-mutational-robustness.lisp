;;; software-mutational-robustness.lisp --- mut-rb across representations
(mapcar #'require '(:software-evolution :memoize :cl-store))
(defpackage :software-mutational-robustness
  (:use :common-lisp :alexandria :metabang-bind :curry-compose-reader-macros
        :software-evolution :software-evolution-utility
        :split-sequence :memoize :cl-store :cl-ppcre))
(in-package :software-mutational-robustness)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *test* "./bin/test.sh"
  "The standard sorter test script.")

(defvar *clang* (from-file (make-instance 'clang) "sorters/merge_c.c")
  "The original program represented using Clang.")

(defvar *cil* (from-file (make-instance 'cil) "sorters/merge_c.c")
  "The original program represented using Cil.")

(defvar *llvm* (from-file (make-instance 'llvm) "sorters/merge_c.ll")
  "The original program represented using LLVM.")

#+(or )
(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(defun neutralp (variant)
  (with-temp-file (bin)
    (ignore-errors
      (and
       (phenome variant :bin bin)
       (multiple-value-bind (stdout stderr errno)
           (shell "~a ~a" *test* bin)
         (declare (ignorable stderr))
         (and (zerop errno) (= (parse-integer stdout))))))))

(defun software-mutational-robustness (variant)
  )
