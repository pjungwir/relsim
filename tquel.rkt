#lang racket

;; TQuel-style temporal relations.
;;
;; Difference from the rest of relsim: in this model, each *attribute* (not
;; the tuple) carries its own valid-time, and a valid-time is a set of all
;; timestamps (a multirange), not a single interval. See README §Notes.
;;
;; The wire types reuse relsim's `rel` and `tuple-desc` structs — a TQuel
;; rel is just a rel whose tuple values are `tsattr` records.

(provide
 (struct-out tsattr)
 multirange-empty?
 multirange-canonical
 multirange-overlaps?
 multirange-union
 multirange-intersection
 multirange-subtract
 rel->tquel
 tquel->rel
 temporal-union/tquel
 temporal-except/tquel
 temporal-cartesian-product/tquel
 temporal-select/tquel
 temporal-project/tquel)

(require "relsim.rkt")

;; ---------------------------------------------------------------------------
;; Multiranges
;; ---------------------------------------------------------------------------

;; A multirange is a list of (s . e) half-open ranges. The canonical form is
;; sorted by start and has no touching/overlapping members. All exported
;; multirange operations return canonical results.

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

;; ---------------------------------------------------------------------------
;; tsattr — a value paired with its valid-time multirange
;; ---------------------------------------------------------------------------

(struct tsattr (val valid-at)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc tsa port mode)
     (fprintf port "~a ~a" (tsattr-val tsa) (tsattr-valid-at tsa)))])

;; ---------------------------------------------------------------------------
;; Local helpers (mirrors of private ones in relsim.rkt)
;; ---------------------------------------------------------------------------

(define (list-remove lst i)
  (append (take lst i) (drop lst (+ i 1))))

(define (field-index desc field)
  (or (index-of (tuple-desc-fields desc) field)
      (error 'tquel "no such field: ~a" field)))

;; ---------------------------------------------------------------------------
;; Conversions
;; ---------------------------------------------------------------------------

;; rel->tquel: drop valid-attr from desc; wrap each remaining field's
;; value in a tsattr whose valid-at is a singleton multirange containing the
;; tuple's original valid-at interval.
(define (rel->tquel r valid-attr)
  (define desc (rel-desc r))
  (define i (field-index desc valid-attr))
  (define new-desc (tuple-desc (list-remove (tuple-desc-fields desc) i)))
  (define new-tuples
    (for/list ([t (in-list (rel-tuples r))])
      (define vs (tuple-values t))
      (define vt (multirange-canonical (list (list-ref vs i))))
      (apply tuple
             (for/list ([v (in-list vs)] [j (in-naturals)]
                        #:when (not (= j i)))
               (tsattr v vt)))))
  (rel new-desc new-tuples))

;; tquel->rel: append valid-attr to the desc and decompose each TQuel tuple
;; into one regular tuple per maximal time-segment over which the set of
;; valid-at attributes is constant. Attributes that aren't valid in the
;; segment are NULL ('()). Segments where no attribute is valid are skipped.
(define (tquel->rel sr valid-attr)
  (define desc (rel-desc sr))
  (define fields (tuple-desc-fields desc))
  (define new-desc (tuple-desc (append fields (list valid-attr))))
  (define new-tuples
    (apply append
           (for/list ([t (in-list (rel-tuples sr))])
             (tquel-tuple->rows t))))
  (rel new-desc new-tuples))

;; Decompose one TQuel tuple into a list of regular tuple values.
(define (tquel-tuple->rows t)
  (define vs (tuple-values t))
  (define vals (map tsattr-val vs))
  (define vts (map tsattr-valid-at vs))
  ;; Collect every endpoint across every attribute's valid-at — these are
  ;; the only places where the "currently valid" set of attributes can change.
  (define endpoints
    (sort (remove-duplicates
           (apply append
                  (for/list ([vt (in-list vts)])
                    (apply append
                           (for/list ([r (in-list vt)])
                             (list (car r) (cdr r)))))))
          <))
  (cond
    [(or (null? endpoints) (null? (cdr endpoints))) '()]
    [else
     (filter values
             (for/list ([s (in-list endpoints)]
                        [e (in-list (cdr endpoints))])
               (define seg (cons s e))
               (define cells
                 (for/list ([v (in-list vals)] [vt (in-list vts)])
                   (cond
                     [(multirange-covers-segment? vt seg) v]
                     [else '()])))
               (cond
                 [(andmap null? cells) #f]
                 [else (apply tuple (append cells (list seg)))])))]))

;; True if some range in mr fully contains seg (which lies between two
;; adjacent endpoints, so it's either entirely inside one range or not at all).
(define (multirange-covers-segment? mr seg)
  (for/or ([r (in-list mr)])
    (and (<= (car r) (car seg)) (>= (cdr r) (cdr seg)))))

;; ---------------------------------------------------------------------------
;; TQuel operators
;; ---------------------------------------------------------------------------

;; In TQuel's algebra, temporal cartesian product is just regular
;; cartesian product: attributes from both sides concatenate, each keeps
;; its own valid-at. The "effective" valid-time of a result row only
;; emerges later (snapshot or tquel->rel) as the intersection of all
;; attribute valid-ats.
(define temporal-cartesian-product/tquel cartesian-product)

;; Select is independent of the temporal dimension — predicate over tuples.
(define temporal-select/tquel select)

;; Project: keep only the named fields, then merge any tuples that share a
;; val-vector by per-attribute union of their valid-ats. Without the merge,
;; project could produce two output tuples with identical attribute values
;; but disjoint valid-times — exactly the duplicate TQuel's data model
;; avoids. A merged row is dropped only when *every* kept attribute has an
;; empty valid-at (matching temporal-except/tquel's drop rule).
(define (temporal-project/tquel fields r)
  (define desc (rel-desc r))
  (define indices (map (lambda (f) (field-index desc f)) fields))
  (define grouped
    (group-by-val
     (for/list ([t (in-list (rel-tuples r))])
       (define vs (tuple-values t))
       (map (lambda (i) (list-ref vs i)) indices))))
  (rel (tuple-desc fields)
       (filter values
               (for/list ([entry (in-list grouped)])
                 (define vts (cdr entry))
                 (cond
                   [(andmap multirange-empty? vts) #f]
                   [else
                    (apply tuple
                           (for/list ([v (in-list (car entry))]
                                      [vt (in-list vts)])
                             (tsattr v vt)))])))))

;; Group a list of tsattr-vectors by val tuple; merge same-val members by
;; per-attribute multirange-union. Returns ((vals . vts) ...) in input order
;; of first occurrence.
(define (group-by-val tsattr-vectors)
  (define order '())
  (define grouped (make-hash))
  (for ([row (in-list tsattr-vectors)])
    (define vals (map tsattr-val row))
    (define vts (map tsattr-valid-at row))
    (cond
      [(hash-has-key? grouped vals)
       (hash-set! grouped vals
                  (for/list ([e (in-list (hash-ref grouped vals))]
                             [v (in-list vts)])
                    (multirange-union e v)))]
      [else
       (set! order (cons vals order))
       (hash-set! grouped vals vts)]))
  (for/list ([vals (in-list (reverse order))])
    (cons vals (hash-ref grouped vals))))

;; Bag union with merging: same val-vector → per-attribute valid-at union.
(define (temporal-union/tquel r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'temporal-union/tquel "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define rows
    (append (rel-tuples r1) (rel-tuples r2)))
  (define grouped
    (group-by-val (map tuple-values rows)))
  (rel (rel-desc r1)
       (for/list ([entry (in-list grouped)])
         (apply tuple
                (for/list ([v (in-list (car entry))]
                           [vt (in-list (cdr entry))])
                  (tsattr v vt))))))

;; Difference: for each r1 row, find r2 rows with the matching val tuple and
;; subtract their valid-ats per-attribute (column-by-column). Drop the row
;; only if *every* attribute's valid-at becomes empty — a row with some
;; emptied attributes and some surviving ones still represents real
;; assertions about the surviving ones, and stays.
;;
;; This is the literal per-attribute reading of TQuel's model — each
;; (attribute, val, valid-at) triple is an independent assertion, and S
;; cancels R's triple wherever they match. It is intentionally NOT snapshot-
;; equivalent to set-theoretic difference: it can over-subtract when two
;; attributes share a val but the tuples that contain them only co-occur
;; over a smaller window. See identities.rkt for the consequence.
(define (temporal-except/tquel r1 r2)
  (unless (equal? (rel-desc r1) (rel-desc r2))
    (error 'temporal-except/tquel "TupleDescs do not match: ~a vs ~a"
           (rel-desc r1) (rel-desc r2)))
  (define minus-by-val (make-hash))
  (for ([t (in-list (rel-tuples r2))])
    (define vs (tuple-values t))
    (define vals (map tsattr-val vs))
    (define vts (map tsattr-valid-at vs))
    (cond
      [(hash-has-key? minus-by-val vals)
       (hash-set! minus-by-val vals
                  (for/list ([e (in-list (hash-ref minus-by-val vals))]
                             [v (in-list vts)])
                    (multirange-union e v)))]
      [else
       (hash-set! minus-by-val vals vts)]))
  (define new-tuples
    (filter values
            (for/list ([t (in-list (rel-tuples r1))])
              (define vs (tuple-values t))
              (define vals (map tsattr-val vs))
              (define vts (map tsattr-valid-at vs))
              (define minus (hash-ref minus-by-val vals #f))
              (define new-vts
                (cond
                  [minus
                   (for/list ([vt (in-list vts)] [m (in-list minus)])
                     (multirange-subtract vt m))]
                  [else vts]))
              (cond
                [(andmap multirange-empty? new-vts) #f]
                [else
                 (apply tuple
                        (for/list ([v (in-list vals)] [vt (in-list new-vts)])
                          (tsattr v vt)))]))))
  (rel (rel-desc r1) new-tuples))
