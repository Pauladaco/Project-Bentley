#lang racket
;projeto lp-2016 paula rafael e waldo
**Função X calcula a soma de um interavalo dado através de ponteiros numa lista.**
(define (X lst ptr1 ptr2)
  (define (auxX lst ptr1 ptr2 counter)
    (if (equal? ptr1 ptr2)
        counter
        (auxX lst ptr1 (- ptr2 1) (+ counter (list-ref lst (- ptr2 1))))))
  (auxX lst ptr1 ptr2 (list-ref lst (- ptr1 1))))

**Função MaxSoFar gera a maior soma possível de uma sublista de tamanho qualquer na lista.**
(define (MaxSoFar lst)
  (let ((p '()))
    (if (null? lst)
        '()
        (for ((i (+ (length lst) 1))
              #:when (> i 0))
          (for ((j (range i (+ (length lst) 1))))
            (begin (set! p (append p (list (X lst i j))))
                   (print p))))))

1ª mudança no código 1:

(define (MaxSoFar lst)
  (let ((p '()))
    (if (null? lst)
        '()
        (for/list ((i (+ (length lst) 1))
              #:when (> i 0))
          (for/list ((j (range i (+ (length lst) 1))))
            (begin (set! p (X lst i j))
                   (list p)))))))
(Problema: Geramos uma lista de somas possíveis, mas ainda não somente a maior soma.)
                   
;;;Código original do artigo:
;MaxSoFar :-- 0.0
;for L := 1 to N do
;for U := L to N do
;Sum := 0. O
;for I := L to U do
;Sum := Sum + X[I]
;/* Sum now contains the
;sum of X[L..U] */
;MaxSoFar := max(MaxSoFar, Sum) 

;(require racket/trace)
;(trace X)
