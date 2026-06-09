#lang racket

;; Stress-test the claim that with temporal-cartesian-product/drop-old,
;; the identity  Q × (R − S) = (Q × R) − (Q × S)  holds.
;;
;; Why we expect it to hold (proof sketch). For a given output key (q, r),
;; let
;;   Q_q  = ⋃ { qt  : (q, qt)  ∈ Q }
;;   S_r  = ⋃ { st  : (r, st)  ∈ S }
;; In other words, Q_q is all the valid-times that q appeared in Q,
;; and S_r is all the valid-times that r appeared in S.
;;
;; The surviving valid-time set contributed by an input (qt, rt) pair is:
;;
;;   LHS  Q × (R − S)        :  qt ∩ (rt − S_r)
;;                          = (qt ∩ rt) − S_r
;;     Intersect and different associate: If we start with qt, then keep what's
;;     in rt and lose what's in S_r, it doesn't matter the order of operations.
;;
;;   RHS  (Q × R) − (Q × S)  :  (qt ∩ rt) − M
;;     where M is the union, over the temporal-except key (q, r), of all
;;     (qt' ∩ st) values produced by Q × S, i.e. M = Q_q ∩ S_r.
;;
;; These two sets are equal: t ∈ qt implies t ∈ Q_q, so for any t in qt ∩ rt,
;; t ∈ S_r ⇔ t ∈ Q_q ∩ S_r. range-subtract-many returns a canonical sorted
;; disjoint decomposition, so equal time-sets give equal piece counts -
;; the bags line up too, not just the snapshot sets.
;;
;; Two properties of the operator pair make this work, and breaking either
;; should reintroduce counterexamples:
;;   (a) /drop-old replaces both inputs' valid-at columns with the single
;;       intersection qt ∩ st, leaving no leftover source-time columns that
;;       would mismatch in the outer temporal-except's key.
;;   (b) temporal-except is set-style in time: every matching right-side
;;       range is unioned into a single minus-set per key. That's what
;;       makes M = Q_q ∩ S_r.

(require "../relsim.rkt")

(define Qd (tuple-desc '(q-id valid-at)))
(define Rd (tuple-desc '(r-id valid-at)))

(define (lhs Q R S)
  (temporal-cartesian-product/drop-old
   'valid-at Q (temporal-except 'valid-at R S)))

(define (rhs Q R S)
  (temporal-except
   'valid-at
   (temporal-cartesian-product/drop-old 'valid-at Q R)
   (temporal-cartesian-product/drop-old 'valid-at Q S)))

(define (check label Q R S)
  (define L (lhs Q R S))
  (define R* (rhs Q R S))
  ;; Compare bag-equal by sorting tuple-values.
  (define (rows r) (sort (map tuple-values (rel-tuples r))
                         (lambda (a b) (string<? (format "~s" a) (format "~s" b)))))
  (define ok? (equal? (rows L) (rows R*)))
  (printf "~a: ~a~n" label (if ok? "EQUAL" "DIFFER"))
  (unless ok?
    (printf "  LHS:~n")
    (print-rel L)
    (printf "  RHS:~n")
    (print-rel R*)))

;; ---- Case 1: the canonical one (already known to pass) ----
(check "canonical"
       (rel Qd (list (tuple 'q1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 10)))))

;; ---- Case 2: Q has a "gap" in valid time ----
(check "Q has gap"
       (rel Qd (list (tuple 'q1 '(0 . 5)) (tuple 'q1 '(10 . 15))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 10)))))

;; ---- Case 3: multiple Q rows with same q-id but different times ----
(check "Q has 2 rows same id"
       (rel Qd (list (tuple 'q1 '(0 . 20)) (tuple 'q1 '(10 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(5 . 15)))))

;; ---- Case 4: R has duplicate rows ----
(check "R has duplicates"
       (rel Qd (list (tuple 'q1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30)) (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(10 . 20)))))

;; ---- Case 5: S has duplicates (would double-subtract in bag) ----
(check "S has duplicates"
       (rel Qd (list (tuple 'q1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(10 . 20)) (tuple 'r1 '(10 . 20)))))

;; ---- Case 6: multiple S rows that fully cover R ----
(check "S fully covers R"
       (rel Qd (list (tuple 'q1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 15)) (tuple 'r1 '(10 . 30)))))

;; ---- Case 7: R has overlapping pieces for same r-id ----
(check "R overlapping pieces same id"
       (rel Qd (list (tuple 'q1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 20)) (tuple 'r1 '(10 . 30))))
       (rel Rd (list (tuple 'r1 '(15 . 25)))))

;; ---- Case 8: Q trims S: Q is narrower than S ----
(check "Q narrower than S"
       (rel Qd (list (tuple 'q1 '(0 . 10))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 15)))))

;; ---- Case 9: Q overlap with S "fits the gap" ----
(check "Q two rows with gap; S fits gap"
       (rel Qd (list (tuple 'q1 '(0 . 5)) (tuple 'q1 '(10 . 15))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 10)))))

;; ---- Case 10: 3-way multiple Q rows + multiple S rows ----
(check "Q×S with multiple rows on each side"
       (rel Qd (list (tuple 'q1 '(0 . 30)) (tuple 'q2 '(10 . 40))))
       (rel Rd (list (tuple 'r1 '(0 . 40)) (tuple 'r2 '(5 . 35))))
       (rel Rd (list (tuple 'r1 '(8 . 12)) (tuple 'r2 '(20 . 25)))))

;; ---- Case 11: empties ----
(check "empty Q"
       (rel Qd '())
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 10)))))
(check "empty R"
       (rel Qd (list (tuple 'q1 '(0 . 20))))
       (rel Rd '())
       (rel Rd (list (tuple 'r1 '(5 . 10)))))
(check "empty S"
       (rel Qd (list (tuple 'q1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd '()))

;; ---- Case 12: touching ranges (no overlap by SQL OVERLAPS) ----
(check "touching ranges"
       (rel Qd (list (tuple 'q1 '(0 . 10)) (tuple 'q1 '(10 . 20))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 15)))))

;; ---- Case 13: Q has time range that doesn't reach R at all ----
(check "Q time disjoint from R"
       (rel Qd (list (tuple 'q1 '(50 . 60))))
       (rel Rd (list (tuple 'r1 '(0 . 20))))
       (rel Rd (list (tuple 'r1 '(5 . 10)))))

;; ---- Case 14: R/S with extra non-time column ----
(define Rd2 (tuple-desc '(r-id color valid-at)))
(check "extra non-time column"
       (rel Qd (list (tuple 'q1 '(0 . 20))))
       (rel Rd2 (list (tuple 'r1 "red" '(0 . 20)) (tuple 'r2 "blue" '(0 . 20))))
       (rel Rd2 (list (tuple 'r1 "red" '(5 . 15)))))

;; ---- Case 15: Q with non-time column ----
(define Qd2 (tuple-desc '(q-id grade valid-at)))
(check "Q with non-time column"
       (rel Qd2 (list (tuple 'q1 'A '(0 . 20)) (tuple 'q1 'B '(10 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(5 . 25)))))

;; ---- Case 16: Bag bombing: Q,R,S all dup the same row ----
(check "Q,R,S all duplicate"
       (rel Qd (list (tuple 'q1 '(0 . 30)) (tuple 'q1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(0 . 30)) (tuple 'r1 '(0 . 30))))
       (rel Rd (list (tuple 'r1 '(10 . 20)) (tuple 'r1 '(10 . 20)))))

;; ---- Case 17: random fuzz ----
(define (rand-range)
  (define s (random 0 20))
  (define e (+ s 1 (random 0 20)))
  (cons s e))
(define (rand-rel desc id-syms n)
  (rel desc (for/list ([_ (in-range n)])
              (tuple (list-ref id-syms (random 0 (length id-syms)))
                     (rand-range)))))

(define fails 0)
(for ([i (in-range 5000)])
  (define Q (rand-rel Qd '(q1 q2 q3) (random 0 6)))
  (define R (rand-rel Rd '(r1 r2 r3) (random 0 6)))
  (define S (rand-rel Rd '(r1 r2 r3) (random 0 6)))
  (define L (lhs Q R S))
  (define R* (rhs Q R S))
  (define (rows r) (sort (map tuple-values (rel-tuples r))
                         (lambda (a b) (string<? (format "~s" a) (format "~s" b)))))
  (unless (equal? (rows L) (rows R*))
    (set! fails (+ fails 1))
    (printf "FUZZ FAIL #~a~n" i)
    (printf "  Q:~n") (print-rel Q)
    (printf "  R:~n") (print-rel R)
    (printf "  S:~n") (print-rel S)
    (printf "  LHS:~n") (print-rel L)
    (printf "  RHS:~n") (print-rel R*)))
(printf "fuzz: ~a/5000 failures~n" fails)
