(load "src/perf-opt.lisp")
(in-package :perf-opt)

(setf *test-fmt* "./bin/test.sh ~a -t 12000 -p")

(defvar *base* "results/heritability"
  "Place to hold results.")

(defvar *energy-model*
  '((:cycles           . 33.1)
    (:instructions     . 12.4)
    (:cache-references . 13.9)
    (:page-faults      . 8.2))
  "Fitness function, evolution attempts to minimize this energy model.")

(defvar *traits* (mapcar #'car *energy-model*)
  "Phenotypic traits of interest.")

(defvar *beta* (mapcar [#'list {* -1} #'cdr] *energy-model*)
  "Selection gradients for the traits of interest.")

;; get a large diverse population
(defvar *pop* (restore "results/energy-1/120833.store"))

;; calculate mean_{P} and V_{P} for each trait
(defvar *pop-stats*
  (mapcar (lambda (trait)
            (let ((vals (mapcar [{aget trait} #'stats] *pop*)))
              (list trait (mean vals) (variance vals))))
          *traits*))
(store *pop-stats* (format nil "~a/pop-stats.store" *base*))

;; pick 100 random sires from the population
(defvar *sires* (loop :for i :below 100 :collect
                   (random-elt *pop*)))
(store *sires* (format nil "~a/sires.store" *base*))

;; for each sire generate 100 neutral offspring
;; slow enough we should use a bunch of threads
(require :eager-future2)(use-package :eager-future2)
(advise-thread-pool-size 48)
(defun pmapcar (f list)
  "Parallel map (from http://marijnhaverbeke.nl/pcall/)."
  (let ((result (mapcar (lambda (n) (pexec (funcall f n))) list)))
    (map-into result #'yield result)))

(defvar *offspring*
  (pmapcar (lambda (sire)
             (let (neutral)
               (loop :until (= (length neutral) 100) :do
                  (let* ((new (copy sire))
                         (stats (progn (mutate new) (test new))))
                    (when (ignore-errors (and (zerop (aget :exit stats))
                                              (zerop (aget :error stats))))
                      (push stats neutral))))
               (cons sire neutral)))
           *sires*))
(store *offspring* (format nil "~a/offspring.store" *base*))

(defvar *stats*
  (apply #'append
         (mapcar (lambda-bind ((sire . offspring))
                   (mapcar {cons (cons :sire (stats sire))} offspring))
                 *offspring*)))

;; calculate the matrix of genetic correlations
(defun covariance (a b)
  (/ (reduce #'+ (mapcar #'*
                         (mapcar {- _ (mean a)} a)
                         (mapcar {- _ (mean b)} b)))
     (- (length a) 1)))

(defun correlation (a b)
  (/ (covariance a b)
     (sqrt (* (variance a) (variance b)))))

(defun relation (sire-trait offspring-trait)
  (correlation (mapcar [{aget sire-trait} {aget :sire}] *stats*)
               (mapcar {aget offspring-trait} *stats*)))

(loop :for row :in *traits* :collect
             (loop :for col :in *traits* :collect
                (relation row col)))
;; => not quite right
;; ((0.702695 0.6535714 0.6414558 0.05302034)
;;  (0.7050594 0.6557561 0.64346075 0.052906547)
;;  (0.70497364 0.65567297 0.6433836 0.05301587)
;;  (0.70511377 0.65569395 0.6433856 0.05255836))

;; calculate Delta-Z given matrix G and *beta*
(defvar *G* '((0.702695 0.6535714 0.6414558 0.05302034)
              (0.7050594 0.6557561 0.64346075 0.052906547)
              (0.70497364 0.65567297 0.6433836 0.05301587)
              (0.70511377 0.65569395 0.6433856 0.05255836)))

(defvar *B* (mapcar [#'list #'cdr] *energy-model*))

(defun matrix-multiply (matrix1 matrix2)
  (mapcar
   (lambda (row)
     (apply #'mapcar
            (lambda (&rest column)
              (apply #'+ (mapcar #'* row column)))
            matrix2))
   matrix1))

(matrix-multiply *B* *G*)


;;; more analysis
(defvar *offspring*
  (restore "results/heritability/offspring.store"))

(with-open-file (out "/tmp/off.csv" :direction :output)
  (let ((fields '(:CYCLES :INSTRUCTIONS :CACHE-REFERENCES :PAGE-FAULTS
                  :BRANCHES :BRANCH-MISSES :TASK-CLOCK)))
    (mapc {format out "~{~a~^,~}~%"}
          (mapcar (lambda (ind) (mapcar {aget _ ind} fields))
                  (apply #'append (mapcar #'cdr *offspring*))))))

(defvar *G*
  '((     1 0.9780 0.9676 0.2745)
    (0.9780      1 0.9955 0.4428)
    (0.9676 0.9955      1 0.4691)
    (0.2745 0.4428 0.4691      1)))

(matrix-multiply *G* *B*)
;; => ((60.927734) (62.240204) (62.118378) (29.297161))
