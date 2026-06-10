#lang racket

;; Range-based temporal relational operators. Each tuple carries a valid-time
;; *range* (an (s . e) pair) under a named attribute; these operators combine
;; rows by overlap/intersection/subtraction of those ranges.

(require "core.rkt"
         "ranges.rkt"
         "multiranges.rkt"
         "relops.rkt")

(provide range-join
         range-join/rename
         range-cartesian-product
         range-cartesian-product/overwrite-old
         range-cartesian-product/drop-old
         range-cartesian-product/rename-old
         range-select
         range-except
         range-division)

;; Range join: like join, but additionally requires the named valid-time
;; range attribute (a (s . e) pair) to overlap on both sides. The result desc
;; is (left-fields ++ right-fields ++ valid-attr), i.e. an extra column with
;; the same name is appended whose value is the intersection range.
(define (range-join pred valid-attr r1 r2)
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define i1 (field-index d1 valid-attr))
  (define i2 (field-index d2 valid-attr))
  (define new-desc
    (tuple-desc (append (tuple-desc-fields d1)
                        (tuple-desc-fields d2)
                        (list valid-attr))))
  (define rows
    (for*/list ([t1 (in-list (rel-tuples r1))]
                [t2 (in-list (rel-tuples r2))]
                #:when (pred t1 t2)
                [ri (in-value (range-intersection
                               (list-ref (tuple-values t1) i1)
                               (list-ref (tuple-values t2) i2)))]
                #:when ri)
      (make-tuple-from-list
       (append (tuple-values t1) (tuple-values t2) (list ri)))))
  (rel new-desc rows))

(define (range-join/rename pred valid-attr r1 r2)
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define i1 (field-index d1 valid-attr))
  (define i2 (field-index d2 valid-attr))
  (define d1prime (tuple-desc (list-set (tuple-desc-fields d1) i1 (string->symbol (string-append "old-" (symbol->string valid-attr))))))
  (define d2prime (tuple-desc (list-set (tuple-desc-fields d2) i2 (string->symbol (string-append "old-" (symbol->string valid-attr))))))
  (define new-desc
    (tuple-desc (append (tuple-desc-fields d1prime)
                        (tuple-desc-fields d2prime)
                        (list valid-attr))))
  (define rows
    (for*/list ([t1 (in-list (rel-tuples r1))]
                [t2 (in-list (rel-tuples r2))]
                #:when (pred t1 t2)
                [ri (in-value (range-intersection
                               (list-ref (tuple-values t1) i1)
                               (list-ref (tuple-values t2) i2)))]
                #:when ri)
      (make-tuple-from-list
       (append (tuple-values t1) (tuple-values t2) (list ri)))))
  (rel new-desc rows))

;; Range cartesian product: every pair of tuples whose valid-time ranges
;; overlap. Result desc matches range-join's: left ++ right ++ valid-attr,
;; with the intersection range in the appended column.
(define (range-cartesian-product valid-attr r1 r2)
  (range-join (lambda (_ __) #t) valid-attr r1 r2))

;; Like range-cartesian-product, but instead of appending the intersection
;; as a third valid-attr column, both input rels' valid-attr columns are
;; replaced with the intersection value. Result desc is just left ++ right
;; (still with valid-attr appearing once on each side).
(define (range-cartesian-product/overwrite-old valid-attr r1 r2)
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define i1 (field-index d1 valid-attr))
  (define i2 (field-index d2 valid-attr))
  (define rows
    (for*/list ([t1 (in-list (rel-tuples r1))]
                [t2 (in-list (rel-tuples r2))]
                [vs1 (in-value (tuple-values t1))]
                [vs2 (in-value (tuple-values t2))]
                [ri (in-value (range-intersection (list-ref vs1 i1)
                                                  (list-ref vs2 i2)))]
                #:when ri)
      (make-tuple-from-list
       (append (list-set vs1 i1 ri) (list-set vs2 i2 ri)))))
  (rel (concat-desc d1 d2) rows))

(define (range-cartesian-product/drop-old valid-attr r1 r2)
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define i1 (field-index d1 valid-attr))
  (define i2 (field-index d2 valid-attr))
  (define d1prime (tuple-desc (list-remove (tuple-desc-fields d1) i1)))
  (define rows
    (for*/list ([t1 (in-list (rel-tuples r1))]
                [t2 (in-list (rel-tuples r2))]
                [vs1 (in-value (tuple-values t1))]
                [vs2 (in-value (tuple-values t2))]
                [ri (in-value (range-intersection (list-ref vs1 i1)
                                                  (list-ref vs2 i2)))]
                #:when ri)
      (make-tuple-from-list
       (append (list-remove vs1 i1) (list-set vs2 i2 ri)))))
  (rel (concat-desc d1prime d2) rows))

(define (range-cartesian-product/rename-old valid-attr r1 r2)
  (range-join/rename (lambda (_ __) #t) valid-attr r1 r2))

;; Range select: an alias for `select`. Kept for naming symmetry with the
;; other range-* operators.
(define range-select select)

;; Range except: like except, but tuples are matched on every field other
;; than valid-attr, and the valid-attr range from each matching r2 row is
;; subtracted from r1's range. A single input row may produce 0, 1, or many
;; output rows (range splits). Both rels must share a TupleDesc and that desc
;; must contain valid-attr.
(define (range-except valid-attr r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'range-except "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define d (rel-desc r1))
  (define i (field-index d valid-attr))
  (define (key-of vs) (append (take vs i) (drop vs (add1 i))))
  ;; Group r2 ranges by non-valid-attr key.
  (define minus-by-key (make-hash))
  (for ([t (in-list (rel-tuples r2))])
    (define vs (tuple-values t))
    (hash-update! minus-by-key (key-of vs)
                  (lambda (rs) (cons (list-ref vs i) rs))
                  '()))
  (define rows
    (apply append
           (for/list ([t (in-list (rel-tuples r1))])
             (define vs (tuple-values t))
             (define minus (hash-ref minus-by-key (key-of vs) '()))
             (define survivors
               (range-subtract-many (list-ref vs i) minus))
             (for/list ([s (in-list survivors)])
               (make-tuple-from-list
                (append (take vs i) (cons s (drop vs (add1 i)))))))))
  (rel d rows))

;; Range division: the temporal analogue of `division`, computed snapshot by
;; snapshot (sequenced semantics). For a candidate key k, k is valid at an
;; instant t exactly when, for every divisor value s that is valid at t, the
;; combined tuple (k, s) is valid at t in R. The result valid-time is bounded
;; to the divisor's lifespan (the union of the divisor values' valid-times),
;; so an empty divisor yields no rows (unlike non-temporal `division`).
;;
;; The surviving valid-time for k is  active − ⋃_s (S_s − R_{k,s}), where
;; S_s is divisor value s's valid-time, R_{k,s} is (k,s)'s valid-time in R,
;; and active = ⋃_s S_s. The set can be multi-interval, so like `range-except`
;; this emits one row per gapless piece.
(define (range-division valid-attr r s)
  (define dr (rel-desc r))
  (define ds (rel-desc s))
  (define r-fields (tuple-desc-fields dr))
  (define s-fields (tuple-desc-fields ds))
  (define ir (field-index dr valid-attr))
  (define is (field-index ds valid-attr))
  (define s-attrs (remove valid-attr s-fields))
  (for ([f (in-list s-attrs)])
    (unless (member f r-fields)
      (error 'range-division "divisor field ~a not in dividend" f)))
  (define k-fields (filter (lambda (f) (not (member f s-attrs)))
                           (remove valid-attr r-fields)))
  (define (pick t fields targets)
    (define vs (tuple-values t))
    (map (lambda (f) (list-ref vs (index-of fields f))) targets))
  ;; divisor value -> its valid-time (a range lifted to a singleton multirange,
  ;; unioned across rows)
  (define Ti (make-hash))
  (for ([t (in-list (rel-tuples s))])
    (hash-update! Ti (pick t s-fields s-attrs)
                  (lambda (acc) (multirange-union acc (list (list-ref (tuple-values t) is))))
                  '()))
  (define dvals (hash-keys Ti))
  (define active
    (for/fold ([a '()]) ([sv (in-list dvals)]) (multirange-union a (hash-ref Ti sv))))
  ;; (k . s) -> (k,s)'s valid-time in R
  (define Rks (make-hash))
  (for ([t (in-list (rel-tuples r))])
    (hash-update! Rks (cons (pick t r-fields k-fields) (pick t r-fields s-attrs))
                  (lambda (acc) (multirange-union acc (list (list-ref (tuple-values t) ir))))
                  '()))
  (define candidates
    (remove-duplicates (for/list ([t (in-list (rel-tuples r))]) (pick t r-fields k-fields))))
  (define rows
    (apply append
           (for/list ([kv (in-list candidates)])
             (define bad
               (for/fold ([b '()]) ([sv (in-list dvals)])
                 (multirange-union
                  b (multirange-subtract (hash-ref Ti sv) (hash-ref Rks (cons kv sv) '())))))
             (define result-time (multirange-subtract active bad))
             (for/list ([piece (in-list result-time)])
               (make-tuple-from-list (append kv (list piece)))))))
  (rel (tuple-desc (append k-fields (list valid-attr))) rows))
