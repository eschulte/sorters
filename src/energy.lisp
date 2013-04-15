(load "src/perf-opt.lisp")
(in-package :perf-opt)

(setf *test-fmt* "./bin/test.sh ~a -t 12000 -p")

(defvar *orig* (from-file (make-instance 'asm-perf :linker "g++")
                          "sorters/merge_cpp.s"))

(defvar *base* "results/energy-1" "Place to hold results.")

;; power optimization
(defvar *energy-model*
  '((:cycles           . 33.1)
    (:instructions     . 12.4)
    (:cache-references . 13.9)
    (:page-faults      . 8.2)))

(defun neutralp (asm)
  (zerop (aget :error (stats asm))))

(defmethod evaluate ((asm asm-perf))
  (unless (stats asm) (setf (stats asm) (test asm)))
  (or (ignore-errors
        (when (and (neutralp asm)
                   (every [{aget _ (stats asm)} #'car] *energy-model*))
          (reduce (lambda-bind (total (metric . coefficient))
                    (+ total (* (aget metric (stats asm)) coefficient)))
                  *energy-model*
                  :initial-value 0)))
      infinity))

(defun checkpoint ()
  (sb-ext:gc :force t)
  (let ((multi (mapcar #'fitness *population*))
        (edits (mapcar [#'count-cons #'edits] *population*)))
    (with-open-file (out "stats" :direction :output
                         :if-exists :append)
      (format out "~&~{~a~^ ~}~%"
              (mapcar #'float
                      (list *fitness-evals*
                            (if (null multi) 0 (mean multi))
                            (if (null edits) 0 (mean edits)))))))
  (store (extremum *population* *fitness-predicate* :key #'fitness)
         (format nil "~a/~d.store" *base* *fitness-evals*)))


#+run
(progn

(setf *work-dir* "sh-runner/work/")

(setf
 (fitness *orig*) (multi-obj *orig*)
 *max-population-size* (expt 2 10)
 *tournament-size* 4
 *fitness-predicate* #'<
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

(loop :for i :from 1 :to 7 :do
   (sb-thread:make-thread
    (lambda ()
      (evolve #'multi-obj
              :filter #'neutralp
              :period (expt 2 9)
              :period-func checkpoint))
    :name (format nil "opt-~d" i)))
)
