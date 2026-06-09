#lang racket

;; Shared harness for the probe scripts. A probe runs one algebraic identity
;; (an LHS expression vs an RHS expression, each a procedure of some input
;; rels) against a list of hand-picked cases and an optional batch of random
;; fuzz inputs, reporting EQUAL/DIFFER per case plus a fuzz failure count.
;;
;; LHS and RHS are arbitrary: pass any two procedures of the same arity and
;; the harness applies both to each case's inputs and bag-compares the result
;; rels. That is what lets the same harness check `A x (B - C) = (A x B) -
;; (A x C)`, `select` distributing over a product, or anything else.

(require "../../relsim.rkt")

(provide probe
         rand-elt
         rand-range
         rand-multirange)

;; Bag-equality: sort tuple-values so row order doesn't matter.
(define (rel-rows r)
  (sort (map tuple-values (rel-tuples r))
        (lambda (a b) (string<? (format "~s" a) (format "~s" b)))))
(define (bag-equal? a b) (equal? (rel-rows a) (rel-rows b)))

;; probe : run one identity experiment.
;;   title         banner printed above the results
;;   lhs rhs        procedures; (apply lhs inputs) / (apply rhs inputs) -> rel
;;   cases          list of (cons label (list input-rel ...))
;;   #:input-names  names used when printing inputs on a failure
;;   #:gen          thunk -> (list input-rel ...), random inputs for fuzzing
;;   #:trials       number of fuzz trials (default 0, i.e. no fuzzing)
;;   #:max-dumps    cap on how many fuzz failures print their rels (the count
;;                  is still exact; this only limits the noise)
(define (probe title lhs rhs cases
               #:input-names [input-names #f]
               #:gen [gen #f]
               #:trials [trials 0]
               #:max-dumps [max-dumps 3])
  (printf "~n=== ~a ===~n" title)
  (define (name-of i)
    (if (and input-names (< i (length input-names)))
        (list-ref input-names i)
        (format "input ~a" (add1 i))))
  (define (dump inputs L R)
    (for ([in (in-list inputs)] [i (in-naturals)])
      (printf "    ~a:~n" (name-of i))
      (print-rel in))
    (printf "    LHS:~n") (print-rel L)
    (printf "    RHS:~n") (print-rel R))
  ;; Hand-picked cases: always show each result, dump rels on a mismatch.
  (for ([c (in-list cases)])
    (define inputs (cdr c))
    (define L (apply lhs inputs))
    (define R (apply rhs inputs))
    (define ok? (bag-equal? L R))
    (printf "  ~a: ~a~n" (car c) (if ok? "EQUAL" "DIFFER"))
    (unless ok? (dump inputs L R)))
  ;; Fuzz: count every failure, dump only the first `max-dumps`.
  (when (and gen (positive? trials))
    (define fails 0)
    (for ([_ (in-range trials)])
      (define inputs (gen))
      (define L (apply lhs inputs))
      (define R (apply rhs inputs))
      (unless (bag-equal? L R)
        (set! fails (add1 fails))
        (when (<= fails max-dumps)
          (printf "  FUZZ FAIL #~a:~n" fails)
          (dump inputs L R))))
    (printf "  fuzz: ~a/~a failures~n" fails trials))
  (void))

;; ---------------------------------------------------------------------------
;; Random generators for fuzzing
;; ---------------------------------------------------------------------------

(define (rand-elt xs) (list-ref xs (random (length xs))))

;; A random half-open range (s . e) with 0 <= s < e <= ~40.
(define (rand-range)
  (define s (random 0 20))
  (define e (+ s 1 (random 0 20)))
  (cons s e))

;; A random multirange: 0-3 ranges, canonicalized (may be empty).
(define (rand-multirange)
  (multirange-canonical
   (for/list ([_ (in-range (random 0 4))]) (rand-range))))
