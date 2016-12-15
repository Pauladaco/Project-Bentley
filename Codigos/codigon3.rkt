#lang racket
(define (X lista ptr1 ptr2)
  (define (aux-X lista ptr1 ptr2 counter)
    (if (equal? ptr1 ptr2)
        counter
        (aux-X lista ptr1 (- ptr2 1) (+ counter (list-ref lista (- ptr2 1))))))
  (aux-X lista ptr1 ptr2 (list-ref lista (- ptr1 1))))


(define (MaxSoFar1 lst)
  (let ((p 0))
  (if (null? lst)
      empty
      (for ((i (+ (length lst) 1))
                 #:when (> i 0))
        (for ((j (range i (+ (length lst) 1))))
          (if (> (X lst i j) p)
                    (begin (set! p (X lst i j)))
                    empty))))
    p))
