#lang racket

;; Ordinary (non-temporal) relational operators.

(require "core.rkt")

(provide select
         project
         cartesian-product
         join
         semijoin
         antijoin
         union
         intersect
         except
         outer-join)

;; Select: keep tuples for which (pred t) is true.
(define (select pred r)
  (rel (rel-desc r)
       (filter pred (rel-tuples r))))

;; Project: restrict to a subset of fields, in the given order.
(define (project fields r)
  (define old-desc (rel-desc r))
  (define indices (map (lambda (f) (field-index old-desc f)) fields))
  (rel (tuple-desc fields)
       (map (lambda (t)
              (define vs (tuple-values t))
              (make-tuple-from-list (map (lambda (i) (list-ref vs i)) indices)))
            (rel-tuples r))))

;; CartesianProduct: every left tuple paired with every right tuple.
(define (cartesian-product r1 r2)
  (rel (concat-desc (rel-desc r1) (rel-desc r2))
       (for*/list ([t1 (in-list (rel-tuples r1))]
                   [t2 (in-list (rel-tuples r2))])
         (concat-tuples t1 t2))))

;; Join: cartesian product filtered by a binary predicate over (left, right).
(define (join pred r1 r2)
  (rel (concat-desc (rel-desc r1) (rel-desc r2))
       (for*/list ([t1 (in-list (rel-tuples r1))]
                   [t2 (in-list (rel-tuples r2))]
                   #:when (pred t1 t2))
         (concat-tuples t1 t2))))

;; Semijoin: rows of r1 that have at least one match in r2. Desc = r1's desc.
(define (semijoin pred r1 r2)
  (define t2s (rel-tuples r2))
  (rel (rel-desc r1)
       (filter (lambda (t1) (ormap (lambda (t2) (pred t1 t2)) t2s))
               (rel-tuples r1))))

;; Antijoin: rows of r1 that have no match in r2. Desc = r1's desc.
(define (antijoin pred r1 r2)
  (define t2s (rel-tuples r2))
  (rel (rel-desc r1)
       (filter (lambda (t1)
                 (not (ormap (lambda (t2) (pred t1 t2)) t2s)))
               (rel-tuples r1))))

;; Union: append two Rels with identical TupleDescs.
(define (union r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'union "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (rel (rel-desc r1)
       (append (rel-tuples r1) (rel-tuples r2))))

;; Build a multiset of tuples (hash from tuple to count).
(define (tuple-counts ts)
  (define h (make-hash))
  (for ([t (in-list ts)]) (hash-update! h t add1 0))
  h)

;; Intersect: multiset intersection (SQL INTERSECT ALL semantics).
;; A tuple appearing m times in r1 and n times in r2 appears (min m n) times.
(define (intersect r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'intersect "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define counts (tuple-counts (rel-tuples r2)))
  (define kept
    (for/fold ([acc '()]) ([t (in-list (rel-tuples r1))])
      (define c (hash-ref counts t 0))
      (cond
        [(positive? c) (hash-set! counts t (sub1 c)) (cons t acc)]
        [else acc])))
  (rel (rel-desc r1) (reverse kept)))

;; Except: multiset difference (SQL EXCEPT ALL semantics).
;; A tuple appearing m times in r1 and n times in r2 appears (max (- m n) 0) times.
(define (except r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'except "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define counts (tuple-counts (rel-tuples r2)))
  (define kept
    (for/fold ([acc '()]) ([t (in-list (rel-tuples r1))])
      (define c (hash-ref counts t 0))
      (cond
        [(positive? c) (hash-set! counts t (sub1 c)) acc]
        [else (cons t acc)])))
  (rel (rel-desc r1) (reverse kept)))

;; OuterJoin: like join, but unmatched rows survive padded with nulls ('()).
;; #:side controls which side keeps unmatched rows: 'left, 'right, or 'full.
(define (outer-join pred r1 r2 #:side [side 'full])
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define nulls1 (make-list (length (tuple-desc-fields d1)) '()))
  (define nulls2 (make-list (length (tuple-desc-fields d2)) '()))
  (define t1s (rel-tuples r1))
  (define t2s (rel-tuples r2))

  ;; For each left tuple, the list of right tuples it matches.
  (define matched
    (for/list ([t1 (in-list t1s)])
      (cons t1 (filter (lambda (t2) (pred t1 t2)) t2s))))

  ;; Right tuples that matched at least one left tuple (by identity).
  (define matched-r2 (apply append (map cdr matched)))

  (define inner-rows
    (apply append
           (for/list ([m (in-list matched)])
             (define t1 (car m))
             (for/list ([t2 (in-list (cdr m))])
               (concat-tuples t1 t2)))))

  (define left-only-rows
    (for/list ([m (in-list matched)]
               #:when (null? (cdr m)))
      (make-tuple-from-list (append (tuple-values (car m)) nulls2))))

  (define right-only-rows
    (for/list ([t2 (in-list t2s)]
               #:unless (memq t2 matched-r2))
      (make-tuple-from-list (append nulls1 (tuple-values t2)))))

  (define rows
    (case side
      [(full)  (append inner-rows left-only-rows right-only-rows)]
      [(left)  (append inner-rows left-only-rows)]
      [(right) (append inner-rows right-only-rows)]
      [else (error 'outer-join "side must be 'full, 'left, or 'right; got ~a"
                   side)]))

  (rel (concat-desc d1 d2) rows))
