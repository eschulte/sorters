;;; horns.lisp --- Higher Order Random and Neutral Search

;; Copyright (C) 2013  Eric Schulte

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

;; utility
(defmacro all-hold (&rest args)
  `(lambda (x) (and ,@(mapcar (lambda (f) `(funcall ,f x)) args))))

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
                        (test new)
                        (incf *fitness-evals*))))
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

(defun focused-rand-run (source max)
  ;; setup
  (setf
   results nil
   *fitness-evals* 0
   orig (from-file (make-instance 'asm-w/muts) source)
   (fitness orig) (test orig)
   *running* t)
  ;; run
  (loop :for n :below num-threads :do
     (push
      (make-thread
       (lambda ()
         (loop :while (and *running* (< *fitness-evals* (* max budget))) :do
            (let ((new (copy orig)))
              (dotimes (n (1+ (random max))) (mutate new))
              (test new)
              (incf *fitness-evals*))))
       :name (format nil "opt-~d" n))
      threads))
  (mapc #'join-thread threads)
  ;; save
  (store results (make-pathname :directory (pathname-directory source)
                                :name (pathname-name source)
                                :type (format nil "rand-to-~d.store" max))))


;;; Analysis
(defvar path-to-feedgnuplot "/usr/bin/feedgnuplot")

(defun feedgnuplot (list &key domain lines histogram hardcopy)
  (let ((proc
         (#+ccl ccl:run-program
          #+sbcl sb-ext:run-program
          path-to-feedgnuplot
          `(,@(when domain '("--domain"))
              ,@(when lines  '("--lines"))
              ,@(when histogram
                      (list "--exit" "--histogram"
                            (format nil "~d"
                                    (if (numberp histogram) histogram 0))))
              ,@(when hardcopy (list "--hardcopy" hardcopy)))
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

(defvar higher-order-neutral-variants nil)
(defvar interesting-higher-order-neutral-variants nil)

#+(or )
(remove-if-not (all-hold [{> _ 1} #'length {aget :mutations}]
                         [{= 10} {aget :fitness}])
               results)

(defun ancestors (record)
  "Return the evaluated ancestors of RECORD."
  (maplist (lambda (mutations)
             (let ((new (copy orig)))
               (dolist (mut (reverse mutations))
                 (apply-mutation new mut))
               `((:fitness . ,(test new))
                 (:mutations . ,mutations))))
           (cdr (aget :mutations record))))

(defun neutral-ancestors (record)
  (remove-if-not [{= 10} {aget :fitness}] (ancestors record)))

(defun non-neutral-ancestors (record)
  (remove-if [{= 10} {aget :fitness}] (ancestors record)))

;; collect all neutral records with non-neutral ancestors
#+(or )
(make-thread
 (lambda ()
   (loop :for thread-id :below num-threads :do
      (make-thread (lambda ()
                     (loop :until (null higher-order-neutral-variants) :do
                        (let* ((it (pop higher-order-neutral-variants))
                               (ancestors (non-neutral-ancestors it)))
                          (unless (null ancestors)
                            (push `((:base . ,it) (:ancestors . ,ancestors))
                                  interesting-higher-order-neutral-variants)))))
                   :name (format nil "checker-~d" thread-id))))
 :name "batch")

(defun all-ancestors (record-w-non-neuts)
  (let ((ancestor-muts (aget :ancestors record-w-non-neuts)))
    (maplist (lambda (mut-group)
               (let ((fitness
                      (or (aget :fitness (car (member mut-group ancestor-muts
                                                      :test #'tree-equal
                                                      :key {aget :mutations})))
                          10)))
                 `((:fitness . ,fitness) (:mutations . ,mut-group))))
             (aget :mutations (aget :base record-w-non-neuts)))))

(defun really-interesting-transitions (record &aux last results)
  (loop :for ancestor :in (reverse (all-ancestors record)) :do
     (when (and last
                ;; fitness improved
                (> (aget :fitness ancestor)
                   (aget :fitness last))
                ;; not a direct reversion
                (not (some {intersection (cdr (car (aget :mutations ancestor)))}
                           (mapcar #'cdr (cdr (aget :mutations ancestor))))))
       (push ancestor results))
     (setf last ancestor))
  results)
