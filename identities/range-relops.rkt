#lang racket

;; A classical relational-algebra identity says that cartesian product
;; distributes over set difference:
;;
;;     Q × (R − S) = (Q × R) − (Q × S)
;;
;; This file is a counterexample showing the temporal analogue does *not*
;; hold for the range-based temporal operators: substituting a
;; `range-cartesian-product` variant for × and `range-except` for −
;; produces two rels with different contents.
;;
;; Run with: racket identities/range-relops.rkt

(require "../relsim.rkt")

;; All three rels share the same valid-time attribute name `valid-at`, since
;; the temporal operators take that name as an argument.
(define Q-desc (tuple-desc '(q-id valid-at)))
(define R-desc (tuple-desc '(r-id valid-at)))
;; S must share R's desc so (range-except R S) is well-formed.
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
  (range-cartesian-product 'valid-at
                              Q
                              (range-except 'valid-at R S)))

;; RHS: take the cartesian products first, then subtract.
;;   Q × R has one row whose appended intersection column is [0,20).
;;   Q × S has one row whose appended intersection column is [5,10).
;;   `range-except` matches on every field except the *leftmost* valid-at
;;   (i.e. Q's valid-at), so it must also match on R's valid-at column and on
;;   the appended-intersection column. Those differ between the two sides, so
;;   nothing cancels and the (Q × R) row survives unchanged.
(define rhs
  (range-except 'valid-at
                   (range-cartesian-product 'valid-at Q R)
                   (range-cartesian-product 'valid-at Q S)))

(displayln "(range-except 'valid-at R S)")
(print-rel (range-except 'valid-at R S))
(displayln "LHS - (range-cartesian-product Q (range-except R S)):")
(print-rel lhs)

(displayln "(range-cartesian-product 'valid-at Q R)")
(print-rel (range-cartesian-product 'valid-at Q R))
(displayln "(range-cartesian-product 'valid-at Q S)")
(print-rel (range-cartesian-product 'valid-at Q S))
(displayln "RHS - (range-except (range-cartesian-product Q R)")
(displayln "                       (range-cartesian-product Q S)):")
(print-rel rhs)

(displayln (format "Equal? ~a" (equal? (rel-tuples lhs) (rel-tuples rhs))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with range-cartesian-product/overwrite-old")
(displayln "================================================================")
(newline)

;; The /overwrite-old variant doesn't append an extra valid-attr column; instead
;; it overwrites both inputs' valid-attr columns with the intersection value.
;; That removes one redundant column from the desc, but the identity still
;; fails: the *second* valid-attr column (originally R's or S's) carries the
;; intersection too, so rows on the two sides of the outer range-except
;; still don't match.

(define lhs2
  (range-cartesian-product/overwrite-old 'valid-at
                                      Q
                                      (range-except 'valid-at R S)))

(define rhs2
  (range-except 'valid-at
                   (range-cartesian-product/overwrite-old 'valid-at Q R)
                   (range-cartesian-product/overwrite-old 'valid-at Q S)))

(displayln "(range-except 'valid-at R S)")
(print-rel (range-except 'valid-at R S))
(displayln "LHS - (tcp/overwrite-old Q (range-except R S)):")
(print-rel lhs2)

(displayln "(range-cartesian-product/overwrite-old 'valid-at Q R)")
(print-rel (range-cartesian-product/overwrite-old 'valid-at Q R))
(displayln "(range-cartesian-product/overwrite-old 'valid-at Q S)")
(print-rel (range-cartesian-product/overwrite-old 'valid-at Q S))
(displayln "RHS - (range-except (tcp/overwrite-old Q R) (tcp/overwrite-old Q S)):")
(print-rel rhs2)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; range-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire. And
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs2) (rel-tuples rhs2))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with range-cartesian-product/rename-old")
(displayln "================================================================")
(newline)

(define lhs3
  (range-cartesian-product/rename-old 'valid-at
                                      Q
                                      (range-except 'valid-at R S)))

(define rhs3
  (range-except 'valid-at
                   (range-cartesian-product/rename-old 'valid-at Q R)
                   (range-cartesian-product/rename-old 'valid-at Q S)))

(displayln "(range-except 'valid-at R S)")
(print-rel (range-except 'valid-at R S))
(displayln "LHS - (tcp/rename-old Q (range-except R S)):")
(print-rel lhs3)

(displayln "(range-cartesian-product/rename-old 'valid-at Q R)")
(print-rel (range-cartesian-product/rename-old 'valid-at Q R))
(displayln "(range-cartesian-product/rename-old 'valid-at Q S)")
(print-rel (range-cartesian-product/rename-old 'valid-at Q S))
(displayln "RHS - (range-except (tcp/rename-old Q R) (tcp/rename-old Q S)):")
(print-rel rhs3)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; range-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire. And
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs3) (rel-tuples rhs3))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with range-cartesian-product/drop-old")
(displayln "================================================================")
(newline)

(define lhs4
  (range-cartesian-product/drop-old 'valid-at
                                      Q
                                      (range-except 'valid-at R S)))

(define rhs4
  (range-except 'valid-at
                   (range-cartesian-product/drop-old 'valid-at Q R)
                   (range-cartesian-product/drop-old 'valid-at Q S)))

(displayln "(range-except 'valid-at R S)")
(print-rel (range-except 'valid-at R S))
(displayln "LHS - (tcp/drop-old Q (range-except R S)):")
(print-rel lhs4)

(displayln "(range-cartesian-product/drop-old 'valid-at Q R)")
(print-rel (range-cartesian-product/drop-old 'valid-at Q R))
(displayln "(range-cartesian-product/drop-old 'valid-at Q S)")
(print-rel (range-cartesian-product/drop-old 'valid-at Q S))
(displayln "RHS - (range-except (tcp/drop-old Q R) (tcp/drop-old Q S)):")
(print-rel rhs4)

;; Unlike the other three variants, /drop-old makes the identity *hold*! The
;; two inputs' valid-at columns are collapsed to the single intersection, so
;; there's no leftover source-time column to mismatch on. See
;; probe/ranges.rkt for the proof sketch and fuzz stress-test.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs4) (rel-tuples rhs4))))
