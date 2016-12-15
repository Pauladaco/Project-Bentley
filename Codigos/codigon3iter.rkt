#lang racket
(define (MaxSoFar5 lst)
  (define (MaxSoFar-iter lst i j max)
    (cond ((eq? i (length lst)) max)
          ((eq? j (length lst)) (MaxSoFar-iter lst (+ 1 i) (+ 1 i) (if (> (X lst i j) max)
                                                                          (X lst i j)
                                                                          max)))
          (else
           (MaxSoFar-iter lst i (+ j 1) (if (> (X lst i j) max)
                                            (X lst i j)
                                            max)))))
  (MaxSoFar-iter lst 1 1 0))

(define (X lista ptr1 ptr2)
  (define (aux-X lista ptr1 ptr2 counter)
    (if (equal? ptr1 ptr2)
        counter
        (aux-X lista ptr1 (- ptr2 1) (+ counter (list-ref lista (- ptr2 1))))))
  (aux-X lista ptr1 ptr2 (list-ref lista (- ptr1 1))))