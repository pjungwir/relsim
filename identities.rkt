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

(require "relsim.rkt")

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
(displayln "  Same identity, but with temporal-cartesian-product/replace")
(displayln "================================================================")
(newline)

;; The /replace variant doesn't append an extra valid-attr column — instead
;; it overwrites both inputs' valid-attr columns with the intersection value.
;; That removes one redundant column from the desc, but the identity still
;; fails: the *second* valid-attr column (originally R's or S's) carries the
;; intersection too, so rows on the two sides of the outer temporal-except
;; still don't match.

(define lhs2
  (temporal-cartesian-product/replace 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs2
  (temporal-except 'valid-at
                   (temporal-cartesian-product/replace 'valid-at Q R)
                   (temporal-cartesian-product/replace 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/replace Q (temporal-except R S)):")
(print-rel lhs2)

(displayln "(temporal-cartesian-product/replace 'valid-at Q R)")
(print-rel (temporal-cartesian-product/replace 'valid-at Q R))
(displayln "(temporal-cartesian-product/replace 'valid-at Q S)")
(print-rel (temporal-cartesian-product/replace 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/replace Q R) (tcp/replace Q S)):")
(print-rel rhs2)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs2) (rel-tuples rhs2))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with temporal-cartesian-product/rename")
(displayln "================================================================")
(newline)

(define lhs3
  (temporal-cartesian-product/rename 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs3
  (temporal-except 'valid-at
                   (temporal-cartesian-product/rename 'valid-at Q R)
                   (temporal-cartesian-product/rename 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/rename Q (temporal-except R S)):")
(print-rel lhs3)

(displayln "(temporal-cartesian-product/rename 'valid-at Q R)")
(print-rel (temporal-cartesian-product/rename 'valid-at Q R))
(displayln "(temporal-cartesian-product/rename 'valid-at Q S)")
(print-rel (temporal-cartesian-product/rename 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/rename Q R) (tcp/rename Q S)):")
(print-rel rhs3)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs3) (rel-tuples rhs3))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with temporal-cartesian-product/replace-last")
(displayln "================================================================")
(newline)

(define lhs4
  (temporal-cartesian-product/replace-last 'valid-at
                                      Q
                                      (temporal-except 'valid-at R S)))

(define rhs4
  (temporal-except 'valid-at
                   (temporal-cartesian-product/replace-last 'valid-at Q R)
                   (temporal-cartesian-product/replace-last 'valid-at Q S)))

(displayln "(temporal-except 'valid-at R S)")
(print-rel (temporal-except 'valid-at R S))
(displayln "LHS — (tcp/replace-last Q (temporal-except R S)):")
(print-rel lhs4)

(displayln "(temporal-cartesian-product/replace-last 'valid-at Q R)")
(print-rel (temporal-cartesian-product/replace-last 'valid-at Q R))
(displayln "(temporal-cartesian-product/replace-last 'valid-at Q S)")
(print-rel (temporal-cartesian-product/replace-last 'valid-at Q S))
(displayln "RHS — (temporal-except (tcp/replace-last Q R) (tcp/replace-last Q S)):")
(print-rel rhs4)

;; Q × R has second valid-at = [0,20); Q × S has second valid-at = [5,10).
;; temporal-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire — and
;; they don't. The (Q × R) row sails through unchanged, while the LHS sees
;; R − S split into pieces before pairing with Q.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs4) (rel-tuples rhs4))))

