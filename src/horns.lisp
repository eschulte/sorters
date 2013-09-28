;;; horns.lisp --- Higher Order Random and Neutral Search

;; Copyright (C) 2012  Eric Schulte

;;; Commentary:

;; An experiment proposed by Wes.
;;
;; Use both a GA, and random mutation (including higher order mutants
;; with a distribution... maybe one which approximates that found
;; through the GA) to collect neutral mutants.
;;
;; Save *every* mutation tested with the resulting neutrality.

;;; Code:
(defpackage :horns
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :split-sequence
        :cl-store
        :cl-ppcre
        :bordeaux-threads
        :software-evolution
        :software-evolution-utility))
(in-package :horns)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar orig         nil         "Original program.")
(defvar budget      (expt 2 18)  "Fitness budget.")
(defvar threads      nil         "Holds all running threads.")
(defvar num-threads (expt 2 5)   "Number of threads to run simultaneously.")
(defvar results      nil         "Holds all fitness results.")

;; record all mutations for each individual
(defclass asm-w/muts (asm)
  ((mutations :initarg :mutations :accessor mutations :initform nil)))

(defmethod copy ((asm asm-w/muts))
  (with-slots (fitness genome linker flags mutations) asm
    (make-instance (type-of asm)
      :fitness fitness
      :genome (copy-tree genome)
      :linker linker
      :flags flags
      :mutations mutations)))

(defmethod apply-mutation :around ((asm-w/muts asm-w/muts) op)
  (call-next-method) (push op (mutations asm-w/muts))
  asm-w/muts)


;;; GA search for neutral variants
(setf
 *max-population-size* (expt 2 10)   ; large populations for diversity
 *tournament-size* 2                 ; more explore than exploit
 *fitness-predicate* #'>             ; we prefer more tests are passed
 *work-dir* "sh-runner/work"         ; use an external runner
 )

(defun test (asm)
  "Test ASM returning the number of tests passed out of 10."
  (let ((fitness
         (or (ignore-errors
               (with-temp-file (bin)
                 (phenome asm :bin bin)
                 (multiple-value-bind (out err errno)
                     (shell "./bin/test.sh ~a" bin)
                   (declare (ignorable err))
                   (when (zerop errno) (parse-number out)))))
             0)))
    ;; Record mutation and fitness information on every individual tested
    (push (list (cons :mutations (mutations asm)) (cons :fitness fitness))
          results)
    fitness))


;;; TODO: Random Search (needs order distribution from GA)


;;; Run
(defun run (source)
  "Run neutral search starting with SOURCE."
  (setf
   results nil
   *fitness-evals* 0
   orig (from-file (make-instance 'asm-w/muts) source)
   (fitness orig) (test orig)
   *population* (list orig))
  (assert (= 10 (fitness orig)) (orig) "Original is not neutral")
  (loop :for n :below num-threads :do
     (push (make-thread (lambda () (evolve #'test :max-evals budget))
                        :name (format nil "opt-~d" n))
           threads))
  (mapc #'join-thread threads)
  (mapc (lambda (obj type)
          (store obj (make-pathname :directory (pathname-directory source)
                                    :name (pathname-name source)
                                    :type (format nil "~a.store" type))))
        (list results *population* orig)
        (list "muts" "pop" "orig")))
;; (make-thread (lambda () (run "sorters/quick_c.s")) :name "quick_c")
