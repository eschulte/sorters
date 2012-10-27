(use-modules (ice-9 rdelim))

(let recur ((a 32))
  (let ((b (read-char)))
    (unless (eof-object? b)
      (write-char (if (equal? #\space a) (char-upcase b) b))
      (recur b))))
