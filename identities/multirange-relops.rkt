#lang racket

;; A classical relational-algebra identity says that cartesian product
;; distributes over set difference:
;;
;;     Q × (R − S) = (Q × R) − (Q × S)
;;
;; This file is the multirange analogue of identities/range-relops.rkt: it
;; shows the same identity does *not* hold for the multirange-based operators
;; either, substituting a `multirange-cartesian-product` variant for × and
;; `multirange-except` for −. As with ranges, only the /drop-old variant
;; restores the identity.
;;
;; Run with: racket identities/multirange-relops.rkt

(require "../relsim.rkt")

;; Same Q/R/S as the range demo, but each valid-at is a multirange (a list of
;; ranges) rather than a single range.
(define Q-desc (tuple-desc '(q-id valid-at)))
(define R-desc (tuple-desc '(r-id valid-at)))
;; S must share R's desc so (multirange-except R S) is well-formed.
(define S-desc R-desc)

(define Q (rel Q-desc (list (tuple 'q1 '((0 . 20))))))
(define R (rel R-desc (list (tuple 'r1 '((0 . 20))))))
(define S (rel S-desc (list (tuple 'r1 '((5 . 10))))))

(displayln "Q:") (print-rel Q)
(displayln "R:") (print-rel R)
(displayln "S:") (print-rel S)

;; LHS: subtract first, then take the cartesian product.
;;   R − S removes the [5,10) hole from r1's [0,20), leaving the multirange
;;   {[0,5),[10,20)} in a single row (multirange-except never splits rows).
;;   Pairing it with Q yields one row.
(define lhs
  (multirange-cartesian-product 'valid-at
                                Q
                                (multirange-except 'valid-at R S)))

;; RHS: take the cartesian products first, then subtract.
;;   Q × R has one row whose appended intersection column is {[0,20)}.
;;   Q × S has one row whose appended intersection column is {[5,10)}.
;;   `multirange-except` matches on every field except the *leftmost* valid-at
;;   (i.e. Q's valid-at), so it must also match on R's valid-at column and on
;;   the appended-intersection column. Those differ between the two sides, so
;;   nothing cancels and the (Q × R) row survives unchanged.
(define rhs
  (multirange-except 'valid-at
                     (multirange-cartesian-product 'valid-at Q R)
                     (multirange-cartesian-product 'valid-at Q S)))

(displayln "(multirange-except 'valid-at R S)")
(print-rel (multirange-except 'valid-at R S))
(displayln "LHS - (multirange-cartesian-product Q (multirange-except R S)):")
(print-rel lhs)

(displayln "(multirange-cartesian-product 'valid-at Q R)")
(print-rel (multirange-cartesian-product 'valid-at Q R))
(displayln "(multirange-cartesian-product 'valid-at Q S)")
(print-rel (multirange-cartesian-product 'valid-at Q S))
(displayln "RHS - (multirange-except (multirange-cartesian-product Q R)")
(displayln "                         (multirange-cartesian-product Q S)):")
(print-rel rhs)

(displayln (format "Equal? ~a" (equal? (rel-tuples lhs) (rel-tuples rhs))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with multirange-cartesian-product/overwrite-old")
(displayln "================================================================")
(newline)

;; The /overwrite-old variant doesn't append an extra valid-attr column; instead
;; it overwrites both inputs' valid-attr columns with the intersection value.
;; That removes one redundant column from the desc, but the identity still
;; fails: the *second* valid-attr column (originally R's or S's) carries the
;; intersection too, so rows on the two sides of the outer multirange-except
;; still don't match.

(define lhs2
  (multirange-cartesian-product/overwrite-old 'valid-at
                                      Q
                                      (multirange-except 'valid-at R S)))

(define rhs2
  (multirange-except 'valid-at
                   (multirange-cartesian-product/overwrite-old 'valid-at Q R)
                   (multirange-cartesian-product/overwrite-old 'valid-at Q S)))

(displayln "(multirange-except 'valid-at R S)")
(print-rel (multirange-except 'valid-at R S))
(displayln "LHS - (mcp/overwrite-old Q (multirange-except R S)):")
(print-rel lhs2)

(displayln "(multirange-cartesian-product/overwrite-old 'valid-at Q R)")
(print-rel (multirange-cartesian-product/overwrite-old 'valid-at Q R))
(displayln "(multirange-cartesian-product/overwrite-old 'valid-at Q S)")
(print-rel (multirange-cartesian-product/overwrite-old 'valid-at Q S))
(displayln "RHS - (multirange-except (mcp/overwrite-old Q R) (mcp/overwrite-old Q S)):")
(print-rel rhs2)

;; Q × R has second valid-at = {[0,20)}; Q × S has second valid-at = {[5,10)}.
;; multirange-except matches on every column except the leftmost valid-at, so
;; those second-valid-at values must agree for the subtraction to fire. And
;; they don't. The (Q × R) row sails through unchanged, while the LHS already
;; carries the {[0,5),[10,20)} hole from R − S.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs2) (rel-tuples rhs2))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with multirange-cartesian-product/rename-old")
(displayln "================================================================")
(newline)

(define lhs3
  (multirange-cartesian-product/rename-old 'valid-at
                                      Q
                                      (multirange-except 'valid-at R S)))

(define rhs3
  (multirange-except 'valid-at
                   (multirange-cartesian-product/rename-old 'valid-at Q R)
                   (multirange-cartesian-product/rename-old 'valid-at Q S)))

(displayln "(multirange-except 'valid-at R S)")
(print-rel (multirange-except 'valid-at R S))
(displayln "LHS - (mcp/rename-old Q (multirange-except R S)):")
(print-rel lhs3)

(displayln "(multirange-cartesian-product/rename-old 'valid-at Q R)")
(print-rel (multirange-cartesian-product/rename-old 'valid-at Q R))
(displayln "(multirange-cartesian-product/rename-old 'valid-at Q S)")
(print-rel (multirange-cartesian-product/rename-old 'valid-at Q S))
(displayln "RHS - (multirange-except (mcp/rename-old Q R) (mcp/rename-old Q S)):")
(print-rel rhs3)

;; Same story as /overwrite-old: the renamed old-valid-at columns differ
;; between the Q × R and Q × S rows, so the outer multirange-except can't
;; match them and nothing cancels.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs3) (rel-tuples rhs3))))

(newline)
(displayln "================================================================")
(displayln "  Same identity, but with multirange-cartesian-product/drop-old")
(displayln "================================================================")
(newline)

(define lhs4
  (multirange-cartesian-product/drop-old 'valid-at
                                      Q
                                      (multirange-except 'valid-at R S)))

(define rhs4
  (multirange-except 'valid-at
                   (multirange-cartesian-product/drop-old 'valid-at Q R)
                   (multirange-cartesian-product/drop-old 'valid-at Q S)))

(displayln "(multirange-except 'valid-at R S)")
(print-rel (multirange-except 'valid-at R S))
(displayln "LHS - (mcp/drop-old Q (multirange-except R S)):")
(print-rel lhs4)

(displayln "(multirange-cartesian-product/drop-old 'valid-at Q R)")
(print-rel (multirange-cartesian-product/drop-old 'valid-at Q R))
(displayln "(multirange-cartesian-product/drop-old 'valid-at Q S)")
(print-rel (multirange-cartesian-product/drop-old 'valid-at Q S))
(displayln "RHS - (multirange-except (mcp/drop-old Q R) (mcp/drop-old Q S)):")
(print-rel rhs4)

;; Unlike the other three variants, /drop-old makes the identity *hold*! The
;; two inputs' valid-at columns are collapsed to the single intersection, so
;; there's no leftover source-time column to mismatch on, and the subtraction
;; on both sides reduces to the same multirange.
(displayln (format "Equal? ~a" (equal? (rel-tuples lhs4) (rel-tuples rhs4))))
