(in-package :software-evolution)
(use-package :curry-and-compose-reader-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-and-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

(defvar *fitness-predicate* #'<
  "Descending order because we want to minimize run time.")

(defvar *test* "../../bin/large-test.sh"
  "The standard sorter test script.")

(defvar *orig* (from-file (make-instance 'asm) "sorters/merge_file_c.s")
  "The original program.")

(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(setf *max-population-size* 256)

(setf *tournament-size* 4)

(defmethod evaluate ((variant asm))
  (with-temp-file (file)
    (phenome variant :bin file)
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a 2>&1" *test* file)
      (declare (ignorable stderr))
      (or (ignore-errors
            (when (zerop exit)
              (with-input-from-string (in stdout)
                (read in nil nil))))
          infinity))))

(defun test (variant)
  (incf *fitness-evals*)
  (evaluate variant))

(memoize #'test :key [#'genome #'first])

;; Run -- this will just run forever
#+run
(progn
  (setf (fitness *orig*) (test *orig*))
  (setf *population* (repeatedly *max-population-size* (copy *orig*)))
  (sb-thread:make-thread
   (lambda ()
     (evolve #'test
             :period 512
             :period-func (lambda ()
                            (store
                             *population*
                             (format nil "pops/~d.store" *fitness-evals*)))))
   :name "opt")
  (loop :for i :upto 24 :do
     (sb-thread:make-thread
      (lambda ()
        (evolve #'test
                :period 512
                :period-func (lambda ()
                               (store
                                *population*
                                (format nil "pops/~d.store" *fitness-evals*)))))
      :name (format nil "opt-~d" i))))
