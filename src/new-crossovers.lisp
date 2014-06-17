;;; new-crossovers.lisp --- Demonstrating new versions of crossover

;; Copyright (C) 2014  Eric Schulte

;;; Commentary:

;; Specifically synaptic and similarity crossover between
;; heterogeneous populations.

;;; Code:
(in-package :goa)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar compilers '(:gcc :clang))
(defvar flags '(:0 :1 :2 :3 :s))

;; Read in and annotate the originals
(defvar mergers
  (let (mergers)
    (loop :for compiler :in compilers :do
       (loop :for flag :in flags :do
          (let ((it (from-file (make-instance 'asm)
                               (format nil "../sorters/_merge_c_~a_O~a.s"
                                       (string-downcase (symbol-name compiler))
                                       (string-downcase (symbol-name flag))))))
            (setf (genome it)
                  (mapcar {append (list (cons :compiler compiler)
                                        (cons :flag flag))}
                          (genome it)))
            (push it mergers))))
    (reverse mergers)))


;;; ASM Evaluation
(defun functionalp (asm)
  (= (or (ignore-errors
           (with-temp-file (bin)
             (phenome asm :bin bin)
             (multiple-value-bind (out err errno)
                 (shell "bin/test.sh ~a" bin)
               (declare (ignorable err))
               (when (and (zerop errno) out (stringp out))
                 (parse-number out)))))
         0)
     10))

(defun profile (asm)
  (ignore-errors
    (with-temp-file (bin)
      (phenome asm :bin bin)
      (multiple-value-bind (out err errno)
          (shell "bin/test.sh ~a -t 12000 -p" bin)
        (declare (ignorable err))
        (when (zerop errno) (parse-stdout out))))))


;;; Similarity
;; 
;; Don't forget to compose each with `{aget :line}'.

(defun equality-distance (inst-a inst-b)
  "Simplest possible, just test complete equality."
  (if (string= inst-a inst-b) 1 0))

(defun component-distance (inst-a inst-b)
  "Split instructions into components and count the number in common."
  (flet ((components (inst) (split "[\\s,]+" inst )))
    (let ((in-common (length (intersection (components inst-a)
                                           (components inst-b)
                                           :test #'string=))))
      (if (zerop in-common) 0
          ;; The `1+' is required to differentiate 1 from 0 in common
          (- 1 (/ 1 (1+ in-common)))))))

(defun edit-distance (inst-a inst-b)
  "Finest grained, test string edit distance."
  (let ((edits (levenshtein-distance inst-a inst-b)))
    (if (zerop edits) 1 (/ 1 edits))))


;;; Experiment
;;
;; For all combinations of;
;; - distinct pairs from mergers
;; - similarity metric
;; - crossover operation (with multiple synapsis context sizes)
;;
;; evaluate the functionality of multiple crossover applications.
;; This should give an idea of how well the new crossovers allow
;; heterogeneous operations to produce functional offspring from
;; heterogeneous parents.

(require :eager-future2)       ; Used to parallelize experimental runs
(eager-future2:advise-thread-pool-size 24)

(defun pairs (list)
  "All pairs from LIST."
  (loop :for i :below (length list) :append
     (loop :for j :in (append (subseq list 0 i) (subseq list (1+ i))) :collect
        (cons (nth i list) j))))

(defun name (asm)
  "Print a name for ASM based on the compiler and flags used."
  (let ((first (car (genome asm))))
    (format nil "~a-~a"
            (string-downcase (symbol-name (aget :compiler first)))
            (symbol-name (aget :flag first)))))

(defun runs (a b cross-op &key (number-of-runs 512))
  "Apply CROSS-OP to A and B multiple times, returning children w/functionality."
  (loop :for run :below number-of-runs :collect
     (let ((mutant (funcall cross-op a b)))
       (setf (fitness mutant) (functionalp mutant))
       mutant)))

(defvar *experiment-futures* nil
  "Collect all experimental futures.")

(setf *work-dir* "../sh-runner/work/")

(defun do-run ()
  ;; distinct pairs from mergers
  (loop :for pair :in (pairs mergers) :do
     ;; similarity metric
     (loop :for metric
        :in '(edit-distance component-distance equality-distance) :do
        ;; crossover operation (with multiple synapsis context sizes)
        (loop :for crossover :in '(synapsing-crossover similarity-crossover) :do
           (loop :for context :in '(1 2 3 4) :do
              (when (or (equal crossover 'synapsing-crossover)
                        (equal context 1))
                (let* ((name (format nil "~a-~a-~a-~a-~a"
                                     (name (car pair)) (name (cdr pair))
                                     (symbol-name metric)
                                     (symbol-name crossover)
                                     context))
                       (path (make-pathname :name name
                                            :type "store"
                                            :directory '(:RELATIVE
                                                         ".."
                                                         "results"
                                                         "new-crossover"))))
                  (unless (probe-file path)
                    (format t "running ~S~%" name)
                    (handler-case
                        (flet ((cross (a b)
                                 (case crossover
                                   (synapsing-crossover
                                    (synapsing-crossover
                                     a b :test metric :context context))
                                   (similarity-crossover
                                    (similarity-crossover
                                     a b :test metric)))))
                          (store (runs (car pair) (cdr pair) #'cross) path))
                      (error (e) (format t "Caught error ~S~%" e)))))))))))

(defun do-collect ()
  (let (results)
    ;; distinct pairs from mergers
    (loop :for pair :in (pairs mergers) :do
       ;; similarity metric
       (loop :for metric
          :in '(edit-distance component-distance equality-distance) :do
          ;; crossover operation (with multiple synapsis context sizes)
          (loop :for crossover :in '(synapsing-crossover similarity-crossover) :do
             (loop :for context :in '(1 2 3 4) :do
                (when (or (equal crossover 'synapsing-crossover)
                          (equal context 1))
                  (let* ((name (format nil "~a-~a-~a-~a-~a"
                                       (name (car pair)) (name (cdr pair))
                                       (symbol-name metric)
                                       (symbol-name crossover)
                                       context))
                         (path (make-pathname :name name
                                              :type "store"
                                              :directory '(:RELATIVE
                                                           ".."
                                                           "results"
                                                           "new-crossover"))))
                    (when (probe-file path)
                      (push (list pair metric crossover context (count-if #'fitness (restore path)))
                            results))))))))
    results))

;; just ran
(defvar *results* (do-collect))

;; TODO: save to disk
(store *results* "/tmp/new-crossovers.store")
