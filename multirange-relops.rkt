#lang racket

;; Multirange-based temporal relational operators. These mirror the range-*
;; operators in range-relops.rkt, but each tuple's valid-time attribute is a
;; *multirange* (a list of (s . e) ranges) rather than a single range. Rows
;; combine by multirange overlap/intersection/subtraction.
;;
;; Two ranges either overlap or don't, and subtracting one range from another
;; can split it into several pieces; range-except therefore emits one output
;; row per piece. A multirange already represents a set of intervals, so the
;; multirange-* operators keep a single value per row: multirange-except
;; subtracts and yields one (possibly multi-interval) multirange, not a row
;; per piece.

(require "core.rkt"
         "multiranges.rkt"
         "relops.rkt")

(provide multirange-join
         multirange-join/rename
         multirange-cartesian-product
         multirange-cartesian-product/overwrite-old
         multirange-cartesian-product/drop-old
         multirange-cartesian-product/rename-old
         multirange-select
         multirange-except
         multirange-division)

;; Multirange join: like join, but additionally requires the named valid-time
;; multirange attribute to overlap on both sides (a non-empty intersection).
;; The result desc is (left-fields ++ right-fields ++ valid-attr): an extra
;; column with the same name is appended whose value is the intersection
;; multirange.
(define (multirange-join pred valid-attr r1 r2)
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
                [mi (in-value (multirange-intersection
                               (list-ref (tuple-values t1) i1)
                               (list-ref (tuple-values t2) i2)))]
                #:unless (multirange-empty? mi))
      (make-tuple-from-list
       (append (tuple-values t1) (tuple-values t2) (list mi)))))
  (rel new-desc rows))

(define (multirange-join/rename pred valid-attr r1 r2)
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
                [mi (in-value (multirange-intersection
                               (list-ref (tuple-values t1) i1)
                               (list-ref (tuple-values t2) i2)))]
                #:unless (multirange-empty? mi))
      (make-tuple-from-list
       (append (tuple-values t1) (tuple-values t2) (list mi)))))
  (rel new-desc rows))

;; Multirange cartesian product: every pair of tuples whose valid-time
;; multiranges overlap. Result desc matches multirange-join's: left ++ right
;; ++ valid-attr, with the intersection multirange in the appended column.
(define (multirange-cartesian-product valid-attr r1 r2)
  (multirange-join (lambda (_ __) #t) valid-attr r1 r2))

;; Like multirange-cartesian-product, but instead of appending the
;; intersection as a third valid-attr column, both input rels' valid-attr
;; columns are replaced with the intersection value. Result desc is just
;; left ++ right (still with valid-attr appearing once on each side).
(define (multirange-cartesian-product/overwrite-old valid-attr r1 r2)
  (define d1 (rel-desc r1))
  (define d2 (rel-desc r2))
  (define i1 (field-index d1 valid-attr))
  (define i2 (field-index d2 valid-attr))
  (define rows
    (for*/list ([t1 (in-list (rel-tuples r1))]
                [t2 (in-list (rel-tuples r2))]
                [vs1 (in-value (tuple-values t1))]
                [vs2 (in-value (tuple-values t2))]
                [mi (in-value (multirange-intersection (list-ref vs1 i1)
                                                       (list-ref vs2 i2)))]
                #:unless (multirange-empty? mi))
      (make-tuple-from-list
       (append (list-set vs1 i1 mi) (list-set vs2 i2 mi)))))
  (rel (concat-desc d1 d2) rows))

(define (multirange-cartesian-product/drop-old valid-attr r1 r2)
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
                [mi (in-value (multirange-intersection (list-ref vs1 i1)
                                                       (list-ref vs2 i2)))]
                #:unless (multirange-empty? mi))
      (make-tuple-from-list
       (append (list-remove vs1 i1) (list-set vs2 i2 mi)))))
  (rel (concat-desc d1prime d2) rows))

(define (multirange-cartesian-product/rename-old valid-attr r1 r2)
  (multirange-join/rename (lambda (_ __) #t) valid-attr r1 r2))

;; Multirange select: an alias for `select`. Kept for naming symmetry with the
;; other multirange-* operators.
(define multirange-select select)

;; Multirange except: like except, but tuples are matched on every field other
;; than valid-attr, and the valid-attr multiranges from all matching r2 rows
;; are unioned and subtracted from r1's multirange. Each input row yields at
;; most one output row (the subtracted multirange), dropped only when that
;; multirange is empty. Both rels must share a TupleDesc and that desc must
;; contain valid-attr.
(define (multirange-except valid-attr r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'multirange-except "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define d (rel-desc r1))
  (define i (field-index d valid-attr))
  (define (key-of vs) (append (take vs i) (drop vs (add1 i))))
  ;; Group r2 multiranges by non-valid-attr key, unioned into one minus-set.
  (define minus-by-key (make-hash))
  (for ([t (in-list (rel-tuples r2))])
    (define vs (tuple-values t))
    (hash-update! minus-by-key (key-of vs)
                  (lambda (m) (multirange-union m (list-ref vs i)))
                  '()))
  (define rows
    (for*/list ([t (in-list (rel-tuples r1))]
                [vs (in-value (tuple-values t))]
                [survivor (in-value
                           (multirange-subtract
                            (list-ref vs i)
                            (hash-ref minus-by-key (key-of vs) '())))]
                #:unless (multirange-empty? survivor))
      (make-tuple-from-list
       (append (take vs i) (cons survivor (drop vs (add1 i)))))))
  (rel d rows))

;; Multirange division: the temporal analogue of `division`, computed snapshot
;; by snapshot (sequenced semantics), with multirange valid-times. For a
;; candidate key k, k is valid at an instant t exactly when, for every divisor
;; value s valid at t, the combined tuple (k, s) is valid at t in R. The result
;; valid-time is bounded to the divisor's lifespan, so an empty divisor yields
;; no rows (unlike non-temporal `division`). Each surviving k is a single row
;; carrying its (possibly multi-interval) result multirange.
(define (multirange-division valid-attr r s)
  (define dr (rel-desc r))
  (define ds (rel-desc s))
  (define r-fields (tuple-desc-fields dr))
  (define s-fields (tuple-desc-fields ds))
  (define ir (field-index dr valid-attr))
  (define is (field-index ds valid-attr))
  (define s-attrs (remove valid-attr s-fields))
  (for ([f (in-list s-attrs)])
    (unless (member f r-fields)
      (error 'multirange-division "divisor field ~a not in dividend" f)))
  (define k-fields (filter (lambda (f) (not (member f s-attrs)))
                           (remove valid-attr r-fields)))
  (define (pick t fields targets)
    (define vs (tuple-values t))
    (map (lambda (f) (list-ref vs (index-of fields f))) targets))
  ;; divisor value -> its valid-time multirange (unioned across rows)
  (define Ti (make-hash))
  (for ([t (in-list (rel-tuples s))])
    (hash-update! Ti (pick t s-fields s-attrs)
                  (lambda (acc) (multirange-union acc (list-ref (tuple-values t) is)))
                  '()))
  (define dvals (hash-keys Ti))
  (define active
    (for/fold ([a '()]) ([sv (in-list dvals)]) (multirange-union a (hash-ref Ti sv))))
  ;; (k . s) -> (k,s)'s valid-time multirange in R
  (define Rks (make-hash))
  (for ([t (in-list (rel-tuples r))])
    (hash-update! Rks (cons (pick t r-fields k-fields) (pick t r-fields s-attrs))
                  (lambda (acc) (multirange-union acc (list-ref (tuple-values t) ir)))
                  '()))
  (define candidates
    (remove-duplicates (for/list ([t (in-list (rel-tuples r))]) (pick t r-fields k-fields))))
  (define rows
    (filter values
            (for/list ([kv (in-list candidates)])
              (define bad
                (for/fold ([b '()]) ([sv (in-list dvals)])
                  (multirange-union
                   b (multirange-subtract (hash-ref Ti sv) (hash-ref Rks (cons kv sv) '())))))
              (define result-time (multirange-subtract active bad))
              (and (not (multirange-empty? result-time))
                   (make-tuple-from-list (append kv (list result-time)))))))
  (rel (tuple-desc (append k-fields (list valid-attr))) rows))
