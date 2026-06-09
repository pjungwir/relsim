#lang racket

;; Range helpers. A range is a half-open pair (s . e) with s < e. These
;; functions are pure interval math: they know nothing about tuples or rels.

(provide range-overlaps
         range-intersection
         range-subtract
         range-subtract-many)

;; range-overlaps: do two half-open ranges share any point?
;; Each range is a pair (s . e) with s < e. Matches SQL OVERLAPS semantics:
;; ranges touching only at an endpoint (e.g. [1,3) and [3,5)) do not overlap.
(define (range-overlaps p1 p2)
  (and (< (car p1) (cdr p2))
       (< (car p2) (cdr p1))))

;; range-intersection: the half-open range common to both inputs, or #f.
;; Matches `range-overlaps`: ranges that only touch at an endpoint return #f.
(define (range-intersection p1 p2)
  (define s (max (car p1) (car p2)))
  (define e (min (cdr p1) (cdr p2)))
  (and (< s e) (cons s e)))

;; range-subtract: subtract b from a, returning a list of 0, 1, or 2 sub-ranges
;; in left-to-right order. Endpoint-touching ranges produce no cut (consistent
;; with range-overlaps).
(define (range-subtract a b)
  (cond
    [(not (range-overlaps a b)) (list a)]
    [else
     (define a-s (car a)) (define a-e (cdr a))
     (define b-s (car b)) (define b-e (cdr b))
     (define left  (and (< a-s b-s) (cons a-s b-s)))
     (define right (and (< b-e a-e) (cons b-e a-e)))
     (filter values (list left right))]))

;; Subtract every range in bs from a, in order. Survivors stay sorted and
;; disjoint as long as a is a single range to start with.
(define (range-subtract-many a bs)
  (for/fold ([survivors (list a)]) ([b (in-list bs)])
    (apply append
           (for/list ([s (in-list survivors)]) (range-subtract s b)))))
