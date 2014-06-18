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

(defvar *asm* (from-file (make-instance 'asm) "sorters/merge_c.s")
  "The original program represented using ASM.")

#+(or )
(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(defun test (variant)
  (with-temp-file (bin)
    (or (ignore-errors
          (and
           (phenome variant :bin bin)
           (multiple-value-bind (stdout stderr errno)
               (shell "~a ~a" *test* bin)
             (declare (ignorable stderr))
             (and (zerop errno) (parse-integer stdout)))))
        -1)))

(defun test-neutrality-and-log (output-file sorter rep)
  "Perform a single neutrality test and log the output."
  (with-open-file (out output-file :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~a ~a " sorter (type-of rep))
    (handler-case
        (multiple-value-bind (variant edit) (mutate (copy rep))
          (format out "~d ~a~%" (test variant) edit))
      (error (e) (format out "~d ~a~%" -1 e)))))

(defun software-mutational-robustness (output-file &key (times 1000))
  (let* ((representations (list 'clang 'cil 'llvm 'asm))
         (extensions (list "c" "c" "ll" "s"))
         (sorters (list "bubble" "insertion" "merge" "quick"))
         (all (mapcan (lambda (sorter)
                        (mapcar (lambda (rep ext)
                                  (cons sorter
                                        (from-file (make-instance rep)
                                                   (format nil "sorters/~a_c.~a"
                                                           sorter ext))))
                                representations extensions))
                      sorters)))
    (dotimes (n times)
      (mapcar (lambda-bind ((sorter . rep))
                (test-neutrality-and-log output-file sorter rep))
              all))))
