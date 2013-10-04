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

(defmethod crossover :around ((a asm-w/muts) (b asm-w/muts))
  (multiple-value-bind (new-individual crossover-data) (call-next-method)
    (setf (mutations new-individual)
          (list (list :crossover crossover-data (mutations a) (mutations b))))
    (values new-individual crossover-data)))


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
                   (when (and (zerop errno) out (stringp out))
                     (parse-number out)))))
             0)))
    ;; Record mutation and fitness information on every individual tested
    (push (list (cons :mutations (mutations asm)) (cons :fitness fitness))
          results)
    fitness))


;;; Random Search (needs order distribution from GA)
(defvar dist nil "Distribution of individual sizes.")

#+(or )
(setf dist (counts (mapcar [#'ancestor-length {aget :mutations}] results)))

(defun pick-dist ()
  (let ((place (random (reduce #'+ (mapcar #'cdr dist)))))
    (loop :for (size . count) :in dist :do
       (if (> place count)
           (decf place count)
           (return-from pick-dist size)))))


;;; Run
(defun run (source type)
  "Run neutral search starting with SOURCE."
  (assert (member type (list :gp :rand)) (type)
          "Type must be set to :gp or :rand.")
  (setf
   results nil
   *fitness-evals* 0
   orig (from-file (make-instance 'asm-w/muts) source)
   (fitness orig) (test orig))
  (assert (= 10 (fitness orig)) (orig) "Original is not neutral")
  (case type
    (:gp
     (setf *population*
           (loop :for i :below *max-population-size* :collect orig))
     (ignore-errors
       (loop :for n :below num-threads :do
          (push (make-thread (lambda () (evolve #'test :max-evals budget))
                             :name (format nil "opt-~d" n))
                threads))))
    (:rand
     (setf *running* t)
     (ignore-errors
       (loop :for n :below num-threads :do
          (push (make-thread
                 (lambda ()
                   (loop :while (and *running* (< *fitness-evals* budget)) :do
                      (let ((new (copy orig)))
                        (dotimes (n (pick-dist)) (mutate new))
                        (test new))))
                 :name (format nil "opt-~d" n))
                threads)))))
  ;; save the results
  (mapc #'join-thread threads)
  (mapc (lambda (obj type)
          (store obj (make-pathname :directory (pathname-directory source)
                                    :name (pathname-name source)
                                    :type (format nil "~a.store" type))))
        (list results *population* orig)
        (list "muts" "pop" "orig")))

#+(or )
(make-thread (lambda ()
               (mapc {run _ :rand}
                     (list "sorters/quick_c.s"
                           "sorters/bubble_c.s")))
             :name "batch-runner")


;;; Analysis
(defvar path-to-feedgnuplot "/usr/bin/feedgnuplot")

(defun feedgnuplot (list &key domain lines histogram)
  (let ((proc
         (#+ccl ccl:run-program
          #+sbcl sb-ext:run-program
          path-to-feedgnuplot
          `(,@(when domain '("--domain"))
              ,@(when lines  '("--lines"))
              ,@(when histogram
                      (list "--exit" "--histogram"
                            (format nil "~d"
                                    (if (numberp histogram) histogram 0)))))
          :input :stream :wait nil)))
    (with-open-stream (feed
                       #+ccl (ccl:external-process-input-stream proc)
                       #+sbcl (sb-ext:process-input proc))
      (format feed "~{~{~a~^ ~}~^~%~}~%" (mapcar (lambda (el)
                                                   (if (listp el) el (list el)))
                                                 list)))
    proc))

(defun ancestor-length (mut-tree)
  (if mut-tree
      (ecase (caar mut-tree)
        ((:swap :cut :insert)
         (1+ (ancestor-length (cdr mut-tree))))
        (:crossover
         ;; just follow the left branch
         (ancestor-length (third (car mut-tree)))
         ;; follow both (too slow for moderate accuracy improvement)
         ;; (/ (+ (ancestor-length (third (car mut-tree)))
         ;;       (ancestor-length (fourth (car mut-tree))))
         ;;    2)
         ))
      0))
