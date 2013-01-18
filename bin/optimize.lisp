(in-package :software-evolution)

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *fitness-predicate* #'<
  "Descending order because we want to minimize run time.")

(defvar *test* "../../bin/profile"
  "The standard sorter test script.")

(defvar *reps* 50
  "Number of repetitions of the sorting test suite used in profiling.")

(defvar *orig* (from-file (make-instance 'asm) "sorters/merge_c.s")
  "The original program.")

(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(setf *max-population-size* 80)

(defmethod evaluate ((variant asm))
  (with-temp-file (file)
    (phenome variant :bin file)
    (sleep 0.1)                         ; <- minor throttling
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a ~a 2>&1" *test* *reps* file)
      (declare (ignorable stderr))
      (or (ignore-errors
            (when (zerop exit)
              (with-input-from-string (in stdout)
                (read in nil nil))))
          infinity))))

(defun test (variant)
  (incf *fitness-evals*)
  (evaluate variant))

;; Run -- this will just run forever
#+run
(progn
  (setf (fitness *orig*) (test *orig*))
  (setf *population* (repeatedly *max-population-size* (copy *orig*)))
  (sb-thread:make-thread (lambda () (evolve #'test)) :name "opt"))