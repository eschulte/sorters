;;; neutral.lisp --- test sorters for neutrality across representations
(require 'software-evolution)
(in-package :software-evolution)


;;; Independent Variables
(defvar algorithms '("bubble" "insertion" "merge" "quick"))
(defvar languages '("c" "cpp" "hs" "ml"))
(defvar representations '(cil clang))
(defvar compilers '(:gcc :clang)) ;; TODO: icc
(defvar comb-flags '("0" "1" "2" "3" "s"))
(defvar compiler-flags
  '((:gcc
     ("0" . "no optimization")
     ("1" . "perform easy optimizations for a balance of speed and size")
     ("2" . "like O1 but more emphasis on speed w/o increasing size")
     ("3" . "heaviest optimization, focus on speed more than size")
     ("s" . "make a small binary")
     ("-Ofast" . "make a fast binary"))
    (:clang
     ("0" . "no optimization")
     ("1" . "perform easy optimizations for a balance of speed and size")
     ("2" . "like O1 but more emphasis on speed w/o increasing size")
     ("s" . "like O2 but decreases code size more")
     ("z" . "like Os but even more")
     ("3" . "heaviest optimization, focus on speed more than size")
     ("4" . "link-time, objects saved as LLVM and whole program optimized"))))


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


;;; Looking at differences in compiler flags in C
(defvar results-dir "../results/neutrality")

(defun neutrality (alg comp flag &key (runs 500))
  "Test the neutrality of ALG in REP compiled with COMP and FLAG."
  (let* ((cil-compiler comp)
         (orig (from-file (make-instance 'cil)
                          (format nil "../sorters/~a.c" alg)))
         (res (repeatedly runs
                (let ((new (mutate (copy orig))))
                  (setf (fitness new) (test-suite new))
                  new))))
    (format t "~&~a ~a ~a ~a~%" alg comp flag
            (count-if (lambda (it) (= (fitness it) *num-tests*)) res))
    (store res (format nil "~a/~a/~a/~a.store"
                       results-dir alg comp flag))))

(mapcomb #'neutrality algorithms compilers comb-flags)
