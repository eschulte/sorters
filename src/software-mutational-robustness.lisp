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

(defvar *elf* (from-file (make-instance 'elf-x86-sw) "sorters/merge_c")
  "The original program represented using ELF.")

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

(defun do-trace (variant)
  (with-temp-file (trace ".trace")
    (let ((inst (instrument (copy variant) trace)))
      (assert (= 10 (test inst))
              (inst)
              "Variant should pass all tests during tracing.")
      (mapcar #'parse-integer
              (split-sequence #\Newline
                (file-to-string trace) :remove-empty-subseqs t)))))

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

(defun software-mutational-robustness (output-file &key (times 1000) only-reps)
  "Calculate the mutational robustness for all program representations.
Save results to OUTPUT-FILE.  Keyword TIMES specifies the number of
trials for each sorter/representation pair.  Keyword ONLY-REPS limits
the test to the specified representations."
  (let* ((representations (list 'clang 'cil 'llvm 'asm 'elf-x86-sw))
         (extensions (list ".c" ".c" ".ll" ".s" ""))
         (sorters (list "bubble" "insertion" "merge" "quick"))
         (all (mapcan (lambda (sorter)
                        (mapcar (lambda (rep ext)
                                  (cons sorter
                                        (from-file (make-instance rep)
                                                   (format nil "sorters/~a_c~a"
                                                           sorter ext))))
                                representations extensions))
                      sorters)))
    (dotimes (n times)
      (format t "~d~%" n)
      (mapcar (lambda-bind ((sorter . rep))
                (test-neutrality-and-log output-file sorter rep))
              (if only-reps
                  (remove-if-not [{member _ only-reps} #'type-of #'cdr] all)
                  all)))))


;;; TODO: direct comparison Clang vs. Cil
