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
         temporal-join
         temporal-cartesian-product
         temporal-select
         temporal-except
         semijoin
         antijoin
         union
         intersect
         except
         outer-join
         print-rel
         range-overlaps
         range-intersection
         range-subtract)

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

;; Temporal join: like join, but additionally requires the named valid-time
;; range attribute (a (s . e) pair) to overlap on both sides. The result desc
;; is (left-fields ++ right-fields ++ valid-attr), i.e. an extra column with
;; the same name is appended whose value is the intersection range.
(define (temporal-join pred valid-attr r1 r2)
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

;; Temporal cartesian product: every pair of tuples whose valid-time ranges
;; overlap. Result desc matches temporal-join's: left ++ right ++ valid-attr,
;; with the intersection range in the appended column.
(define (temporal-cartesian-product valid-attr r1 r2)
  (temporal-join (lambda (_ __) #t) valid-attr r1 r2))

;; Temporal select: keep tuples for which (pred t) is true AND whose valid-time
;; range overlaps query-range. The valid-attr field of each output tuple is
;; replaced with the intersection of its original range and query-range. Desc
;; is unchanged. Unlike temporal-join (which has two source ranges and appends
;; an intersection column), there is only one source range here, so updating it
;; in place keeps the desc clean.
(define (temporal-select pred valid-attr query-range r)
  (define d (rel-desc r))
  (define i (field-index d valid-attr))
  (define rows
    (for*/list ([t (in-list (rel-tuples r))]
                #:when (pred t)
                [vs (in-value (tuple-values t))]
                [ri (in-value (range-intersection (list-ref vs i) query-range))]
                #:when ri)
      (make-tuple-from-list
       (append (take vs i) (cons ri (drop vs (add1 i)))))))
  (rel d rows))

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

;; Temporal except: like except, but tuples are matched on every field other
;; than valid-attr, and the valid-attr range from each matching r2 row is
;; subtracted from r1's range. A single input row may produce 0, 1, or many
;; output rows (range splits). Both rels must share a TupleDesc and that desc
;; must contain valid-attr.
(define (temporal-except valid-attr r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'temporal-except "TupleDescs do not match: ~a vs ~a"
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

;; ---------------------------------------------------------------------------
;; Pretty-printing
;; ---------------------------------------------------------------------------

(define (cell->string v)
  (cond [(null? v) "NULL"]
        [(string? v) v]
        [else (format "~a" v)]))

;; Print r as an ASCII table with column headers from its TupleDesc.
(define (print-rel r [out (current-output-port)])
  (define headers (map symbol->string (tuple-desc-fields (rel-desc r))))
  (define rows (map (lambda (t) (map cell->string (tuple-values t)))
                    (rel-tuples r)))
  (define widths
    (for/list ([i (in-naturals)] [h (in-list headers)])
      (apply max (string-length h)
             (map (lambda (row) (string-length (list-ref row i))) rows))))
  (define sep
    (string-append "+"
                   (string-join
                    (for/list ([w (in-list widths)])
                      (make-string (+ w 2) #\-))
                    "+")
                   "+"))
  (define (row-line cells)
    (string-append "| "
                   (string-join
                    (for/list ([c (in-list cells)] [w (in-list widths)])
                      (~a c #:min-width w))
                    " | ")
                   " |"))
  (displayln sep out)
  (displayln (row-line headers) out)
  (displayln sep out)
  (for ([row (in-list rows)]) (displayln (row-line row) out))
  (displayln sep out))

;; ---------------------------------------------------------------------------
;; Range helpers
;; ---------------------------------------------------------------------------

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
