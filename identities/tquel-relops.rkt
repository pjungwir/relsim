#lang racket

;; The classical relational-algebra identity
;;
;;     Q × (R − S) = (Q × R) − (Q × S)
;;
;; again, but expressed with the TQuel operators. As with the range-based
;; operators (see range-relops.rkt), the temporal analogue does *not* hold.
;;
;; In TQuel's data model each *attribute* carries its own valid-time,
;; and there is no tuple-level valid-at column. In addition, the valid-time is
;; not an interval, but a set of all valid-times for that tuple (thus avoiding
;; duplicates wrt the non-valid-time attribute values). We can represent that
;; as multiranges, which is nice because we can do the same in Postgres.
;;
;; Cartesian product is plain attribute concatenation; difference subtracts
;; per-attribute valid-ats by matching the val tuple. The identity still
;; fails here: on the LHS, Q's valid-time isn't touched (R-S removes nothing
;; from Q), but on the RHS, Q × S asserts q-id = q1 during [0,20), so the
;; per-attribute difference subtracts that whole window from Q × R's q-id
;; column, even though the (q1, r1) *combination* in Q × S only spans the
;; smaller [5,10) intersection.
;;
;; Run with: racket identities/tquel-relops.rkt

(require "../relsim.rkt")

;; The same Q/R/S as the range-based demonstration, before conversion.
(define Q-desc (tuple-desc '(q-id valid-at)))
(define R-desc (tuple-desc '(r-id valid-at)))
(define S-desc R-desc)

(define Q (rel Q-desc (list (tuple 'q1 '(0 . 20)))))
(define R (rel R-desc (list (tuple 'r1 '(0 . 20)))))
(define S (rel S-desc (list (tuple 'r1 '(5 . 10)))))

(define Q-s (rel->tquel Q 'valid-at))
(define R-s (rel->tquel R 'valid-at))
(define S-s (rel->tquel S 'valid-at))

(define lhs-s
  (temporal-cartesian-product/tquel
   Q-s
   (temporal-except/tquel R-s S-s)))

(define rhs-s
  (temporal-except/tquel
   (temporal-cartesian-product/tquel Q-s R-s)
   (temporal-cartesian-product/tquel Q-s S-s)))

(displayln "(temporal-except/tquel R-s S-s)")
(print-rel (temporal-except/tquel R-s S-s))
(displayln "LHS - (× Q-s (- R-s S-s)):")
(print-rel lhs-s)

(displayln "(temporal-cartesian-product/tquel Q-s R-s)")
(print-rel (temporal-cartesian-product/tquel Q-s R-s))
(displayln "(temporal-cartesian-product/tquel Q-s S-s)")
(print-rel (temporal-cartesian-product/tquel Q-s S-s))
(displayln "RHS - (- (× Q-s R-s) (× Q-s S-s)):")
(print-rel rhs-s)

(displayln (format "Equal? ~a" (equal? lhs-s rhs-s)))
