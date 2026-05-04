#lang racket

(provide (struct-out tuple-desc)
         (struct-out rel)
         tuple
         tuple?
         tuple-values
         tuple-ref
         null?-rel
         select
         project
         cartesian-product
         join
         union
         outer-join)

;; ---------------------------------------------------------------------------
;; Core data types
;; ---------------------------------------------------------------------------

;; A Tuple is a struct holding a list of values aligned with a TupleDesc's
;; fields. The empty list '() represents SQL NULL.
;;
;; The constructor is variadic: (tuple 1 'x "hi") => tuple with values '(1 x "hi").
(struct tuple-internal (values) #:transparent
  #:constructor-name make-tuple-from-list
  #:reflection-name 'tuple)

(define (tuple . vs) (make-tuple-from-list vs))
(define tuple? tuple-internal?)
(define tuple-values tuple-internal-values)

;; A TupleDesc lists the field names (symbols) of a Rel's tuples.
(struct tuple-desc (fields) #:transparent)

;; A Rel is a TupleDesc plus a list of Tuples (duplicates allowed, like SQL).
(struct rel (desc tuples) #:transparent)

;; '() is our null marker.
(define (null?-rel v) (null? v))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (field-index desc field)
  (or (index-of (tuple-desc-fields desc) field)
      (error 'field-index "no such field: ~a" field)))

;; Get a field value from a tuple given the Rel's TupleDesc.
(define (tuple-ref t desc field)
  (list-ref (tuple-values t) (field-index desc field)))

(define (concat-desc d1 d2)
  (tuple-desc (append (tuple-desc-fields d1)
                      (tuple-desc-fields d2))))

(define (concat-tuples t1 t2)
  (make-tuple-from-list (append (tuple-values t1) (tuple-values t2))))

;; ---------------------------------------------------------------------------
;; Relational operators
;; ---------------------------------------------------------------------------

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

;; Union: append two Rels with identical TupleDescs.
(define (union r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'union "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (rel (rel-desc r1)
       (append (rel-tuples r1) (rel-tuples r2))))

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
