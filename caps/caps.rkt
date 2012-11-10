#lang typed/racket

(: cap-forward (Char -> Char))
(define (cap-forward prev)
  (let: ([stdin (current-input-port)]
         [space : Char #\Space]
         [low-a : Char #\a])
    (let: ([curr : (U Char EOF) (read-char stdin)])
      (if (char? curr)
          (begin (display (if (eq? space prev)
                              (char-upcase curr)
                              curr))
                 (cap-forward curr))
          #\newline))))
(display (cap-forward #\Space))
