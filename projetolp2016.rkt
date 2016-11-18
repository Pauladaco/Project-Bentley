#lang racket

;projeto lp-2016 paula rafael e waldo

; Algoritmo O(Nˆ3)

**Função X calcula a soma de um interavalo dado através de ponteiros numa lista.**

(define (X lista ptr1 ptr2)
  (define (aux-X lista ptr1 ptr2 counter)
    (if (equal? ptr1 ptr2)
        counter
        (aux-X lista ptr1 (- ptr2 1) (+ counter (list-ref lista (- ptr2 1))))))
  (aux-X lista ptr1 ptr2 (list-ref lista (- ptr1 1))))

**Função MaxSoFar gera a maior soma possível de uma sublista de tamanho qualquer na lista.**
(define (MaxSoFar lista)
  (let ((p empty)
    (if (null? lista)
        empty
        (for ((i (+ (length lista) 1))
              #:when (> i 0))
          (for ((j (range i (+ (length lista) 1))))
            (begin (set! p (append p (list (X lista i j))))
                   (print p))))))

1ª mudança no código 1:
(define (MaxSoFar lista)
  (let ((p empty)
    (if (null? lista)
        empty
        (for/list ((i (+ (length lista) 1))
              #:when (> i 0))
          (for/list ((j (range i (+ (length lista) 1))))
            (begin (set! p (X lista i j))
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


; função max para nested lists

(define (multi-max array)
  (cond ((pair? array) (max (multi-max (car array)) (multi-max (cdr array))))
        ((number? array) array)
        (else 0)))

; vetores teste

(define beta (list 1 2 3 4 5 6 7 8 9 10))

(define gamma (list 4 10 9 1 3 9 0 20 0))

(define testebeta (max-so-far beta))

(define testegamma (max-so-far gamma))
