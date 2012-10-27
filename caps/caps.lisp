(time
 (with-open-file (out "/dev/null" :direction :output
                      :if-exists :overwrite
                      :element-type '(unsigned-byte 8))
   (with-open-file (in "/tmp/test" :element-type '(unsigned-byte 8))
     (labels ((recur (a) (let ((b (read-byte in nil)))
                           (when b
                             (write-byte (if (and (<= b 122) (>= b 97) (= a 32)) (- b 32) b)
                                         out)
                             (recur b)))))
       (recur 32)))))

;; Evaluation took:
;;   3.589 seconds of real time
;;   3.580000 seconds of total run time (3.580000 user, 0.000000 system)
;;   99.75% CPU
;;   9,658,680,704 processor cycles
;;   0 bytes consed
