# relsim

A small Racket library for building relations and evaluating expressions
in the style of the relational algebra. SQL-flavored: nulls are allowed
(represented as `'()`) and duplicate tuples are preserved.

## Concepts

- **Tuple** — a struct holding a list of values. Construct variadically:
  `(tuple 1 "Alice" 10)`.
- **TupleDesc** — names the fields of the tuples in a Rel:
  `(tuple-desc '(id name dept-id))`.
- **Rel** — a TupleDesc plus a list of Tuples:
  `(rel desc (list (tuple ...) ...))`.

Operators provided: `select`, `project`, `cartesian-product`, `join`,
`semijoin`, `antijoin`, `union`, `intersect`, `except`, `outer-join`.

`union`, `intersect`, and `except` use multiset (SQL `... ALL`) semantics:
duplicates are preserved, and counts combine accordingly (sum, min,
truncated difference). All three require both inputs to share a TupleDesc.

## Using it from a REPL

From this directory, start Racket and require the module:

```
$ racket
Welcome to Racket.
> (require "relsim.rkt")
> (define employees-desc (tuple-desc '(id name dept-id)))
> (define employees
    (rel employees-desc
         (list (tuple 1 "Alice" 10)
               (tuple 2 "Bob"   20)
               (tuple 3 "Carol" 10))))
> (define eng
    (select (lambda (t) (equal? (tuple-ref t employees-desc 'dept-id) 10))
            employees))
> (map tuple-values (rel-tuples eng))
'((1 "Alice" 10) (3 "Carol" 10))
```

A few more examples:

```racket
;; Project to a subset of fields (also works to reorder).
(project '(name) employees)

;; Cartesian product.
(cartesian-product employees depts)

;; Equi-join on dept-id.
(define ed (rel-desc employees))
(define dd (rel-desc depts))
(join (lambda (e d)
        (equal? (tuple-ref e ed 'dept-id)
                (tuple-ref d dd 'dept-id)))
      employees depts)

;; Set-style union (TupleDescs must match; duplicates preserved).
(union r1 r2)

;; Multiset intersect / except (TupleDescs must match).
(intersect r1 r2)
(except r1 r2)

;; Semijoin / antijoin: filter rows of the left rel by existence of a match.
(semijoin pred employees depts)
(antijoin pred employees depts)

;; Full outer join. Use #:side 'left or #:side 'right for one-sided variants.
(outer-join pred employees depts #:side 'full)

;; Pretty-print a Rel as an ASCII table (nulls show as NULL).
(print-rel employees)
```

Nulls are written as `'()`. Padding from an outer join produces `'()`
in the columns of the unmatched side.

## Running the tests

From this directory:

```
$ racket tests.rkt
```

The script exits 0 on success, 1 on any failure. You can also run them
via raco:

```
$ raco test tests.rkt
```
