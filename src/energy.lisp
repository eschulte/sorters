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
  (ignore-errors
    (and (zerop (aget :exit (stats asm)))
         (zerop (aget :error (stats asm))))))

(defmethod evaluate ((asm asm-perf))
  (unless (stats asm)
    (setf (stats asm) (test asm))
    (push (cons *fitness-evals* (stats asm))
          *evaluations*))
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
  ;; population metrics
  (let ((multi (mapcar #'fitness *population*))
        (edits (mapcar [#'count-cons #'edits] *population*)))
    (with-open-file (out (format nil "~a/pop.stats" *base*)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format
       out "~&~{~a~^ ~}~%"
       (mapcar #'float
               (list *fitness-evals*
                     ;; start saving min, median *and* mean for pop stats
                     (if (null multi) 0 (mean multi))
                     (if (null edits) 0 (mean edits))
                     ;; O3 penetration
                     (reduce #'+ (mapcar [{count-if {assoc :O3}} #'genome]
                                         *population*))
                     (reduce #'+ (mapcar [#'length #'genome] *population*)))))))
  (when (zerop (mod *checkpoint-counter* 16))
    ;; individual metrics
    (with-open-file (out (format nil "~a/ind.stats" *base*)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~&~{~S~^~%~}~%" *evaluations*))
    (setf *evaluations* nil)
    (store *population* (format nil "~a/~d.store" *base* *fitness-evals*)))
  (incf *checkpoint-counter*))


#+run
(progn

(setf *work-dir* "sh-runner/work/")

(setf
 (fitness *orig*) (evaluate *orig*)
 *max-population-size* (expt 2 10)
 *tournament-size* 4
 *fitness-predicate* #'<
 *population* (loop :for n :below *max-population-size* :collect (copy *orig*)))

(loop :for i :from 1 :to 7 :do
   (sb-thread:make-thread
    (lambda ()
      (evolve #'evaluate
              :filter #'neutralp
              :period (expt 2 9)
              :period-func #'checkpoint))
    :name (format nil "opt-~d" i)))
)


;;; O3
;; O3 output
;; 0,error
;; 13500443,cycles
;; 19029847,instructions
;; 33400,cache-references
;; 374,page-faults
;; 4172943,branches
;; 122502,branch-misses
;; 6.351270,task-clock

#+O3
(progn
(defvar *O3* '((:cycles           . 13500443)
               (:instructions     . 19029847)
               (:cache-references . 33400)
               (:page-faults      . 374)))

(reduce (lambda-bind (total (metric . coefficient))
          (+ total (* (aget metric *O3*) coefficient)))
        *energy-model*
        :initial-value 0)
;; => 6.833021e8
)

;; (stats *orig*)
;; ((:EXIT . 0) (:ERROR . 0) (:CYCLES . 33136998) (:INSTRUCTIONS . 49626497)
;;  (:CACHE-REFERENCES . 33159) (:PAGE-FAULTS . 375) (:BRANCHES . 9239904)
;;  (:BRANCH-MISSES . 147008) (:TASK-CLOCK . 15.573425))


;;; O3 incorporation
;;
;; Incorporating O3 into the running population, but first we're
;; tagging all of the lines of its genome with markers.
;;
;; We'll also save the best we had before O3 was incorporated.
;;
#+O3-incorporation
(progn
(defvar *pre-best* (extremum *population* #'< :key #'fitness))

(fitness *pre-best*) ;; => 1.1575474e9

*fitness-evals* ;; => 2796737

(push *O3* *population*)
)
