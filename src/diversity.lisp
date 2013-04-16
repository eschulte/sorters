(load "src/perf-opt.lisp")
(in-package :perf-opt)

(setf *test-fmt* "./bin/test.sh ~a")

(defvar *base* "results/diversity" "Place to hold results.")

(defclass asm-diverse (asm-perf)
  ((initial-cmp :initarg :initial-cmp :accessor initial-cmp :initform nil)
   (initial-flg :initarg :initial-flg :accessor initial-flg :initform nil)))

(defmethod copy ((asm asm-diverse)
                 &key (edits (copy-tree (edits asm))) (fitness (fitness asm)))
  (make-instance (type-of asm)
    :edits edits
    :fitness fitness
    :genome (copy-tree (genome asm))
    :linker (linker asm)
    :flags (flags asm)
    :initial-cmp (initial-cmp asm)
    :initial-flg (initial-flg asm)))

(defmethod evaluate ((asm asm-diverse))
  (with-temp-file (bin)
    (phenome asm :bin bin)
    (multiple-value-bind (stdout stderr errno) (shell *test-fmt* bin)
      (declare (ignorable stderr))
      (or (ignore-errors (when (zerop errno) (parse-number stdout)))
          0))))


;;; Diverse individual Generation
;;
;; Generate and label diverse individuals.
;;

;; By compiler
(defvar *cmps-and-flags*
  '((:gcc   O0 O1 O2 O3 Os Ofast)
    (:clang O0 O1 O2 O3 O4 Os Oz)))

(defvar *origs*
  (mapcan
   (lambda-bind ((cmp . flags))
     (mapcar
      (lambda (flag)
        (let ((asm (from-file (make-instance 'asm-diverse
                                :initial-cmp cmp :initial-flg flag)
                              (format nil "sorters/_merge_c_~a_~a.s"
                                      (string-downcase (symbol-name cmp))
                                      (string-capitalize (symbol-name flag))))))
          (setf (genome asm)
                (mapcar [{cons (cons :cmp cmp)} {cons (cons :flag flag)}]
                        (genome asm)))
          asm))
      flags))
   *cmps-and-flags*)
  "List of individuals with genomes tagged by their compiler and flag.")

;; By algorithm
(defvar *by-alg*
  (mapcar {format nil "~a_cpp.s"} '("bubble" "insertion" "merge" "quick")))


;;; form a population and evolve
#+run
(progn
(setf
 *origs* (remove-if-not [{= 10} #'fitness]
                        (mapc (lambda (orig) (setf (fitness orig) (evaluate orig)))
                              *origs*))
 *max-population-size* (expt 2 9)
 *tournament-size* 2
 *fitness-predicate* #'>
 *cross-chance* 3/5
 *population* (loop :for n :below *max-population-size* :by (length *origs*)
                 :append (mapcar #'copy *origs*)))

(evolve #'evaluate :max-evals (expt 2 18))

)
