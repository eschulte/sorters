;;; neutral.lisp --- test sorters for neutrality across representations
(load "~/.sbclrc" :if-does-not-exist nil)
(require 'software-evolution)
(in-package :software-evolution)

(advise-thread-pool-size 40)

(defvar *num-tests* 10)

(defvar *test* "test.sh")

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

(defun neutrality (path &key (runs 1000) linker)
  "Test the neutrality of the assembly program at PATH."
  (let* ((pathname (pathname path))
         (orig (from-file (make-instance 'asm
                            :linker linker)
                          pathname))
         (store (make-pathname
                 :directory (pathname-directory pathname)
                 :name (pathname-name pathname)
                 :type "store")))
    (cond
      ((not (= *num-tests* (test-suite orig))) ;; sanity
       (format t "Sanity check failed: ~a~%" path))
      ((probe-file store) ;; don't overwrite existing results
       (format t "Store file exists: ~a~%" store))
      (t ;; run
       (format t "~&~a ~S~%"
               path
               (/ (count-if (lambda (it) (= (fitness it) *num-tests*))
                            (store (repeatedly runs
                                     (let ((new (mutate (copy orig))))
                                       (setf (fitness new) (test-suite new))
                                       (make-instance 'asm
                                         :fitness (fitness new)
                                         :edits (edits new))))
                                   store))
                  runs))))
    store))
