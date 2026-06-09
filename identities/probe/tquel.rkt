#lang racket

;; Probe experiments for the TQuel operators. Unlike ranges/multiranges, the
;; TQuel cartesian product is plain attribute concatenation (no valid-at
;; column to drop), and difference subtracts per-attribute valid-ats by
;; matching the value tuple. Each attribute is a `tsattr` (a value plus its
;; valid-time multirange).
;;
;; Run with: racket identities/probe/tquel.rkt

(require "harness.rkt"
         "../../relsim.rkt")

;; After rel->tquel the valid-at column is gone and every attribute is a
;; tsattr, so Q is a one-column (q-id) rel and R/S are one-column (r-id) rels.
(define Qd (tuple-desc '(q-id)))
(define Rd (tuple-desc '(r-id)))

(define (q tsa-id mr) (tuple (tsattr tsa-id mr)))
(define (r tsa-id mr) (tuple (tsattr tsa-id mr)))

;; ===========================================================================
;; Experiment 1: does tquel-cartesian-product distribute over tquel-except?
;;   Q × (R − S) = (Q × R) − (Q × S)
;; ===========================================================================
;;
;; This one does NOT hold (see identities/tquel-relops.rkt for the worked
;; counterexample). Per-attribute difference cancels a (value, valid-at)
;; assertion wherever it matches, so on the RHS, Q × S subtracts Q's whole
;; valid-time from Q × R's q-id column even though the (q, r) *combination*
;; only co-occurs over the smaller intersection. The probe is here to confirm
;; the failure and exercise it over many shapes, not to prove a theorem.

(define (dist-lhs Q R S)
  (tquel-cartesian-product Q (tquel-except R S)))

(define (dist-rhs Q R S)
  (tquel-except
   (tquel-cartesian-product Q R)
   (tquel-cartesian-product Q S)))

(define dist-cases
  (list
   ;; canonical counterexample from identities/tquel-relops.rkt
   (list "canonical (expected DIFFER)"
         (rel Qd (list (q 'q1 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((5 . 10))))))
   ;; with empty S the difference does nothing, so both sides are Q × R
   (list "empty S (degenerate, holds)"
         (rel Qd (list (q 'q1 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((0 . 20)))))
         (rel Rd '()))
   ;; S matches r but Q and R never co-occur over the subtracted window
   (list "Q narrower than the subtracted window"
         (rel Qd (list (q 'q1 '((0 . 8)))))
         (rel Rd (list (r 'r1 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((5 . 15))))))
   ;; S's r-id doesn't match R, so R − S = R; both sides are Q × R
   (list "non-matching S (holds)"
         (rel Qd (list (q 'q1 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((0 . 20)))))
         (rel Rd (list (r 'r2 '((5 . 10))))))))

(define (rand-tq-rel desc id-syms n)
  (rel desc (for/list ([_ (in-range n)])
              (tuple (tsattr (rand-elt id-syms) (rand-multirange))))))

(define (dist-gen)
  (list (rand-tq-rel Qd '(q1 q2 q3) (random 0 5))
        (rand-tq-rel Rd '(r1 r2 r3) (random 0 5))
        (rand-tq-rel Rd '(r1 r2 r3) (random 0 5))))

(probe "tquel-cartesian-product distributes over tquel-except"
       dist-lhs dist-rhs dist-cases
       #:input-names '("Q" "R" "S")
       #:gen dist-gen #:trials 5000 #:max-dumps 2)

;; ===========================================================================
;; Experiment 2: does tquel-select distribute over tquel-cartesian-product?
;;   σ_p(Q × R) = (σ_p Q) × R,  where p only inspects Q's column
;; ===========================================================================
;;
;; tquel-cartesian-product is plain cartesian-product and tquel-select is plain
;; select, so a left-only predicate slides past the product exactly as in the
;; classical algebra. q-id is column 0 in both Q and Q × R.

(define (q1? t) (eq? (tsattr-val (list-ref (tuple-values t) 0)) 'q1))

(define (sel-lhs Q R)
  (tquel-select q1? (tquel-cartesian-product Q R)))

(define (sel-rhs Q R)
  (tquel-cartesian-product (tquel-select q1? Q) R))

(define sel-cases
  (list
   (list "mixed q-ids"
         (rel Qd (list (q 'q1 '((0 . 20))) (q 'q2 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((5 . 15))))))
   (list "predicate keeps nothing"
         (rel Qd (list (q 'q2 '((0 . 20)))))
         (rel Rd (list (r 'r1 '((5 . 15))))))
   (list "duplicate q1 rows"
         (rel Qd (list (q 'q1 '((0 . 20))) (q 'q1 '((10 . 30)))))
         (rel Rd (list (r 'r1 '((5 . 15))) (r 'r2 '((0 . 30))))))
   (list "empty R"
         (rel Qd (list (q 'q1 '((0 . 20)))))
         (rel Rd '()))))

(define (sel-gen)
  (list (rand-tq-rel Qd '(q1 q2 q3) (random 0 6))
        (rand-tq-rel Rd '(r1 r2 r3) (random 0 6))))

(probe "tquel-select distributes over tquel-cartesian-product"
       sel-lhs sel-rhs sel-cases
       #:input-names '("Q" "R")
       #:gen sel-gen #:trials 5000)
