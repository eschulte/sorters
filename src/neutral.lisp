#!/usr/bin/sbcl --script
;;; -*- common-lisp -*-
;;; neutral.lisp --- test sorters for neutrality across representations
(require 'software-evolution)
(in-package :software-evolution)


;;; Program evaluation
(defvar *test* "../bin/test.sh")
(defvar *num-tests* 10)

(defun run-test (phenome num)
  (multiple-value-bind (output err-output exit)
      (shell "~a ~a ~a" *test* phenome num)
    (declare (ignorable output err-output))
    (zerop exit)))

(def-memoized-function test-suite (ast)
  (with-temp-file (bin)
    (if (phenome ast :bin bin)
        (count t (loop :for num :below *num-tests* :collect (run-test bin num)))
        0)))

(defun neutrality (path &key (runs 500))
  "Test the neutrality of the assembly program at PATH."
  (let* ((pathname (pathname path))
         (orig (from-file (make-instance 'asm) pathname))
         (store (make-pathname
                 :directory (pathname-directory pathname)
                 :name (pathname-name pathname)
                 :type "store")))
    ;; sanity
    (assert (= *num-tests* (test-suite orig)) (orig)
            "Sanity check on the fitness of the original program")
    ;; run
    (format t "~&~S ~S~%"
            path
            (/ (count-if (lambda (it) (= (fitness it) *num-tests*))
                         (store (repeatedly runs
                                  (let ((new (mutate (copy orig))))
                                    (setf (fitness new) (test-suite new))
                                    new))
                                store))
               runs))
    
    store))

(let ((files (cdr sb-ext:*posix-argv*)))
  (mapcar #'neutrality files))
