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
(defvar *offspring*
  (loop :for sire :in *sires* :collect
     (let (neutral)
       (loop :until (= (length neutral) 100) :do
          (let* ((new (copy sire))
                 (stats (progn (mutate new) (test new))))
            (when (and (zerop (aget :exit stats))
                       (zerop (aget :error stats)))
              (push stats neutral))))
       neutral)))
(store *offspring* (format nil "~a/offspring.store" *base*))

;; calculate V_{G} for each pair of traits
(defvar *combined-traits*
  (loop :for s-trait :in *traits* :collect
     (loop :for o-trait :in *traits* :collect
        (list s-trait o-trait
              (mapcar
               (lambda (sire offspring)
                 (mapcar [{cons (aget s-trait (stats sire))} {aget o-trait}]
                         offspring))
               *sires* *offspring*)))))

;; calculate Delta-Z given matrix G and *beta*

