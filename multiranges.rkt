#lang racket

;; Multirange helpers. A multirange is a list of (s . e) half-open ranges. The
;; canonical form is sorted by start and has no touching/overlapping members.
;; All exported multirange operations return canonical results. Like ranges.rkt,
;; this is pure interval math with no knowledge of tuples or rels.

(require "ranges.rkt")

(provide multirange-empty?
         multirange-canonical
         multirange-union
         multirange-intersection
         multirange-subtract
         multirange-overlaps?)

(define (multirange-empty? mr) (null? mr))

;; Sort by start, then sweep merging members that touch or overlap.
(define (multirange-canonical mr)
  (define sorted (sort mr < #:key car))
  (let loop ([rs sorted] [acc '()])
    (cond
      [(null? rs) (reverse acc)]
      [(null? acc) (loop (cdr rs) (list (car rs)))]
      [else
       (define top (car acc))
       (define next (car rs))
       (cond
         [(<= (car next) (cdr top))
          (define merged (cons (car top) (max (cdr top) (cdr next))))
          (loop (cdr rs) (cons merged (cdr acc)))]
         [else (loop (cdr rs) (cons next acc))])])))

(define (multirange-union m1 m2)
  (multirange-canonical (append m1 m2)))

(define (multirange-intersection m1 m2)
  (multirange-canonical
   (for*/list ([r1 (in-list m1)]
               [r2 (in-list m2)]
               [ri (in-value (range-intersection r1 r2))]
               #:when ri)
     ri)))

(define (multirange-subtract m1 m2)
  (multirange-canonical
   (for/fold ([acc m1]) ([r2 (in-list m2)])
     (apply append
            (for/list ([r1 (in-list acc)]) (range-subtract r1 r2))))))

(define (multirange-overlaps? m1 m2)
  (not (multirange-empty? (multirange-intersection m1 m2))))
