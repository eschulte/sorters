(require :software-evolution)
(in-package :software-evolution)

;; compile insertion.c to assembly
(shell "gcc -S insertion.c")

;; general configuration settings
(setq *pos-test-num* 10)
(setq *neg-test-num* 0)
(setq *test-script* "./test.sh")
(setq original (asm-from-file "insertion.s"))

(unless (= 10 (fitness original))
  (error "Failed sanity check: ~s!=10" (fitness original)))

;; use good path samples
#+apply-path
(progn
  (setq *genome-averaging-keys* '(:pos))
  (apply-path original :pos (samples-from-tracer-file "trace")))


;;; experimentation
(require :eager-future2)
(use-package :eager-future2)

(defun pmapcar (f list)
  "Parallel map (from http://marijnhaverbeke.nl/pcall/)."
  (let ((result (mapcar (lambda (n) (pexec (funcall f n))) list)))
    (map-into result #'yield result)))

(defmacro repeatedly (n &body body)
  (let ((result-sym (gensym)))
    `(let ((,result-sym (loop :for _ :upto ,n :collect (pexec ,@body))))
       (map-into ,result-sym #'yield ,result-sym))))

(defmethod size ((ant software))
  (length (genome ant)))

(defun ant-stats (ant)
  "Return statistics about point ANT in the genotype space."
  (let ((fitness (fitness ant)) neighbors)
    (dotimes (_ 10)
      (let ((neighbor (copy ant)))
        (mutate neighbor)
        (push `((:fitness . ,(fitness neighbor))
                (:history . ,(history neighbor))) neighbors)))
    `((:size      . ,(size ant))
      (:fitness   . ,fitness)
      (:history   . ,(history ant))
      (:neighbors . ,neighbors))))

(defvar *pop* nil
  "Population variable, made global to allow peeking in on runs.")

(defun do-random-walk (dir &key (walks 1000) (steps 100))
  "Run a series of random walks saving results to DIR."
  (dotimes (n walks)
    (store (let ((ant (asm-from-file "insertion.s")))
             (repeatedly steps (prog1 (ant-stats ant) (mutate ant))))
     (merge-pathnames (format nil "rand-walk-~S.store" n) dir))))

#+run-random-walk
(do-random-walk "results/rand-walks")

(defun do-neutral-step (pop &key (size nil) (select #'random-elt))
  (repeatedly (or size (length pop))
    (let ((ant (copy (funcall select pop))))
      (mutate ant)
      (if (= 10 (fitness ant)) ant (copy (random-elt pop))))))

(defun do-neutral-walk (dir &key (popsize 100) (steps 1000))
  "Expand a population in the neutral space saving results to DIR."
  (setf *pop* (do-neutral-step (list (asm-from-file "insertion.s"))
                :size popsize))
  (dotimes (n steps)
    (store (pmapcar #'ant-stats *pop*)
           (merge-pathnames (format nil "neut-pop-~S.store" n) dir))
    (setf *pop* (do-neutral-step *pop*))))

#+run-neutral-walk
(do-neutral-walk "results/neut-walk/")

(defun do-biased-walk (dir &key
                             (popsize 100) (steps 1000)
                             (test #'<) (key #'size) (tournysize 2))
  "Evolve a population in the neutral space biased by TEST and KEY."
  (setf *pop* (do-neutral-step (list (asm-from-file "insertion.s")) popsize))
  (flet ((pick (pop) (first (sort (repeatedly tournysize (random-elt *pop*))
                                  test :key key))))
    (dotimes (n steps)
      (store (pmapcar #'ant-stats *pop*)
             (merge-pathnames (format nil "biased-pop-~S.store" n) dir))
      (setf *pop* (do-neutral-step *pop* :select #'pick)))))

#+run-biased-walk
(do-biased-walk "results/biased-short/")


;; analysis
(defun aget (key lst) (cdr (assoc key lst)))
(defun getter (key) (lambda (it) (aget key it)))
(defun transpose (matrix) (apply #'map 'list #'list matrix))

(defun step-mut-rb (step)
  "Return the mutational robustness of a step from its neighbors."
  (/ (count 10 (mapcar (getter :fitness) (aget :neighbors step))) 10))

(defun walk-stats (walk)
  "Return per-step alists of size, fitness, `step-neighbor-stats'. "
  (mapcar (lambda (step)
            `((:mut-rb   . ,(step-mut-rb step))
              (:genotype . ,(sxhash (aget :history step)))
              (:fitness  . ,(aget :fitness step))
              (:size     . ,(aget :size step))))
          (reverse (copy-tree walk))))

(defun step-stats (steps)
  (flet ((mean-and-stdev (lst) (cons (mean lst) (variance lst))))
    (mapcar
     (lambda (steps)
       `((:neutral   . ,(/ (count 10 (mapcar (getter :fitness) steps)) 101))
         (:fitness   . ,(mean-and-stdev (mapcar (getter :fitness) steps)))
         (:size      . ,(mean-and-stdev (mapcar (getter :size)    steps)))
         (:mut-rb    . ,(mean-and-stdev (mapcar (getter :mut-rb)  steps)))))
     steps)))

(defun by-step (walks)
  "Given a list of walks, return stats organized by step."
  (step-stats (transpose walks)))

(defun to-file (steps file)
  "Dump steps to file as tab separated text."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (loop :for step :in *stats* :do
       (format out "~&~f ~f ~f ~f ~f ~f ~f"
               (aget :neutral step)
               (car (aget :fitness step))
               (cdr (aget :fitness step))
               (car (aget :size step))
               (cdr (aget :size step))
               (car (aget :mut-rb step))
               (cdr (aget :mut-rb step))))))

(defvar *walks* nil
  "The raw walk data read directly from what is stored to disk.")

(defvar *stats* nil
  "Statistics describing the walk data organized by step.")

#+random-walk-results
(progn
  (setf *walks*
        (loop :for i :from 0 :upto 999 :collect
           (walk-stats
            (restore
             (format nil "results/rand-walks/rand-walk-~a.store" i)))))

  (setf *stats* (by-step *walks*))

  (to-file *stats* "results/rand-walks/stats.txt"))

#+neut-walk-results
(progn
  (setf *walks*
        (loop :for i :from 0 :upto 999 :collect
           (walk-stats
            (restore
             (format nil "results/neut-walk/neut-pop-~a.store" i)))))

  (setf *stats* (step-stats *walks*))

  (to-file *stats* "results/neut-walk/stats.txt"))
