#lang racket
(define (X-2 lista)
  (define (aux-X lista counter max)
    (if (empty? lista)
        max
        (aux-X (cdr lista) (+ counter (car lista)) (if (< (+ counter (car lista)) max)
                                                                       max
                                                                        (+ counter (car lista))))))
  (aux-X lista 0 0))

(define (MaxSoFar4 lst)
  (define (MaxSoFar4-iter lst max)
    (if (empty? lst)
        max
        (MaxSoFar4-iter (cdr lst) (if (< (X-2 lst) max)
                                      max
                                      (X-2 lst)))))
  (MaxSoFar4-iter lst 0))
  
  ;;tentei implementar o codigo n², nao tenho certeza se esse codigo tem complexida n²;;
  ;;ele roda mais rapido que o n³ e mais lento que o n;;
