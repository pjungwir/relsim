#lang racket

;; A classical relational-algebra identity says that cartesian product
;; distributes over set difference:
;;
;;     Q × (R − S) = (Q × R) − (Q × S)
;;
;; This file is a counterexample showing the temporal analogue does *not*
;; hold: substituting `temporal-cartesian-product` for × and `temporal-except`
;; for − produces two rels with different contents.
;;
;; Run with: racket identities.rkt

(require "relsim.rkt"
         "tquel.rkt")

;; All three rels share the same valid-time attribute name `valid-at`, since
;; the temporal operators take that name as an argument.
(define Q-desc (tuple-desc '(q-id valid-at)))
(define R-desc (tuple-desc '(r-id valid-at)))
;; S must share R's desc so (temporal-except R S) is well-formed.
(define S-desc R-desc)

(define Q (rel Q-desc (list (tuple 'q1 '(0 . 20)))))
(define R (rel R-desc (list (tuple 'r1 '(0 . 20)))))
(define S (rel S-desc (list (tuple 'r1 '(5 . 10)))))

(displayln "Q:") (print-rel Q)
(displayln "R:") (print-rel R)
(displayln "S:") (print-rel S)

;; LHS: subtract first, then take the cartesian product.
;;   R − S splits r1's [0,20) around the [5,10) hole, giving two pieces.
;;   Pairing each with Q yields two rows.
(define lhs
  (temporal-cartesian-product 'valid-at
                              Q
                              (temporal-except 'valid-at R S)))

;; RHS: take the cartesian products first, then subtract.
;;   Q × R has one row whose appended intersection column is [0,20).
;;   Q × S has one row whose appended intersection column is [5,10).
;;   `temporal-except` matches on every field except the *leftmost* valid-at
;;   (i.e. Q's valid-at), so it must also match on R's valid-at column and on
;;   the appended-intersection column. Those differ between the two sides, so
;;   nothing cancels and the (Q × R) row survives unchanged.
(define rhs
  (temporal-except 'valid-at
                   (temporal-cartesian-product 'valid-at Q R)
                   (temporal-cartesian-product 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (temporal-cartesian-product Q (temporal-except R S)):")
(print-rel lhs)

(displayln "(temporal-cartesian-product 'valid-at Q R)")
(print-rel (temporal-cartesian-product 'valid-at Q R))
(displayln "(temporal-cartesian-product 'valid-at Q S)")
(print-rel (temporal-cartesian-product 'valid-at Q S))
(displayln "RHS — (temporal-except (temporal-cartesian-product Q R)")
(displayln "                       (temporal-cartesian-product Q S)):")
(print-rel rhs)

(displayln (format "Equal? ~a" (equal? (rel-tuples lhs) (rel-tuples rhs))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with temporal-cartesian-product/overwrite-old")
(displayln "================================================================")
(newline)

;; The /overwrite-old variant doesn't append an extra valid-attr column — instead
;; it overwrites both inputs' valid-attr columns with the intersection value.
;; That removes one redundant column from the desc, but the identity still
;; fails: the *second* valid-attr column (originally R's or S's) carries the
;; intersection too, so rows on the two sides of the outer temporal-except
;; still don't match.

(define lhs2
  (temporal-cartesian-product/overwrite-old 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs2
  (temporal-except 'valid-at
                   (temporal-cartesian-product/overwrite-old 'valid-at Q R)
                   (temporal-cartesian-product/overwrite-old 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/overwrite-old Q (temporal-except R S)):")
(print-rel lhs2)

(displayln "(temporal-cartesian-product/overwrite-old 'valid-at Q R)")
(print-rel (temporal-cartesian-product/overwrite-old 'valid-at Q R))
(displayln "(temporal-cartesian-product/overwrite-old 'valid-at Q S)")
(print-rel (temporal-cartesian-product/overwrite-old 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/overwrite-old Q R) (tcp/overwrite-old Q S)):")
(print-rel rhs2)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs2) (rel-tuples rhs2))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with temporal-cartesian-product/rename-old")
(displayln "================================================================")
(newline)

(define lhs3
  (temporal-cartesian-product/rename-old 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs3
  (temporal-except 'valid-at
                   (temporal-cartesian-product/rename-old 'valid-at Q R)
                   (temporal-cartesian-product/rename-old 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/rename-old Q (temporal-except R S)):")
(print-rel lhs3)

(displayln "(temporal-cartesian-product/rename-old 'valid-at Q R)")
(print-rel (temporal-cartesian-product/rename-old 'valid-at Q R))
(displayln "(temporal-cartesian-product/rename-old 'valid-at Q S)")
(print-rel (temporal-cartesian-product/rename-old 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/rename-old Q R) (tcp/rename-old Q S)):")
(print-rel rhs3)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs3) (rel-tuples rhs3))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with temporal-cartesian-product/drop-old")
(displayln "================================================================")
(newline)

(define lhs4
  (temporal-cartesian-product/drop-old 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs4
  (temporal-except 'valid-at
                   (temporal-cartesian-product/drop-old 'valid-at Q R)
                   (temporal-cartesian-product/drop-old 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/drop-old Q (temporal-except R S)):")
(print-rel lhs4)

(displayln "(temporal-cartesian-product/drop-old 'valid-at Q R)")
(print-rel (temporal-cartesian-product/drop-old 'valid-at Q R))
(displayln "(temporal-cartesian-product/drop-old 'valid-at Q S)")
(print-rel (temporal-cartesian-product/drop-old 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/drop-old Q R) (tcp/drop-old Q S)):")
(print-rel rhs4)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs4) (rel-tuples rhs4))))

(newline)
(displayln "================================================================")
(displayln "  Q × (R − S) = (Q × R) − (Q × S)")
(displayln "  Same identity, but with TQuel operators")
(displayln "================================================================")
(newline)

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
;; column — even though the (q1, r1) *combination* in Q × S only spans the
;; smaller [5,10) intersection.

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
(displayln "LHS — (× Q-s (- R-s S-s)):")
(print-rel lhs-s)

(displayln "(temporal-cartesian-product/tquel Q-s R-s)")
(print-rel (temporal-cartesian-product/tquel Q-s R-s))
(displayln "(temporal-cartesian-product/tquel Q-s S-s)")
(print-rel (temporal-cartesian-product/tquel Q-s S-s))
(displayln "RHS — (- (× Q-s R-s) (× Q-s S-s)):")
(print-rel rhs-s)

(displayln (format "Equal? ~a" (equal? lhs-s rhs-s)))

