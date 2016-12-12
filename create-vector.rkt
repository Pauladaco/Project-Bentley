#lang racket
(define (create-vector size range)
  (define (create-vector-aux size range sign count vector)
    (cond ((eq? size count) vector)
          ((eq? sign #t) (create-vector-aux size range (random-boolean) (+ count 1) (append vector (list (random-integer range)))))
          ((eq? sign #f) (create-vector-aux size range (random-boolean) (+ count 1) (append vector (list (- (random-integer range))))))))
  (create-vector-aux size range (random-boolean) 0 '()))


(define (random-boolean)
  (if (< (random) 0.5)
      #t
      #f))


(require (planet williams/science/random-source))