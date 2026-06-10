# relsim

A REPL/library for playing with relational algebra, written in Racket.
Includes temporal operators.

This project lets you build relations and evaluate
relational expressions. It defines all the normal relational operators.
To make it practical, I made it SQL-flavored: nulls are allowed
(represented as `'()`) and duplicate tuples are preserved.

My personal motivation is to experiment with *temporal* relational operators,
and see if they obey the same algebraic identities as their traditional analogues.
This matters for query transformations done by the planner.
For instance, does Cartesian product distribute over difference?:

$$
Q \mathbin{\hat\times} (R \mathbin{\hat-} S) \equiv (Q \mathbin{\hat\times} R) \mathbin{\hat-} (Q \mathbin{\hat\times} S)
$$

According to "TQuel Overview" by Richard Snodgrass in *Temporal Databases: Theory,
Design, and Implementation* (1993), this identity does *not* hold in TQuel.
What about other temporal models?
See [the identities folder](/identities/README.md) for what I've learned so far.

## Concepts

- **Tuple** - a struct holding a list of values. Construct variadically:
  `(tuple 1 "Alice" 10)`.
- **TupleDesc** - names the fields of the tuples in a Rel:
  `(tuple-desc '(id name dept-id))`.
- **Rel** - a TupleDesc plus a list of Tuples:
  `(rel desc (list (tuple ...) ...))`.

Operators provided: `select`, `project`, `cartesian-product`, `join`,
`range-join`, `range-cartesian-product`, `range-select`,
`range-except`, `multirange-join`, `multirange-cartesian-product`,
`multirange-select`, `multirange-except`, `semijoin`, `antijoin`, `union`,
`intersect`, `except`, `outer-join`.

`union`, `intersect`, and `except` use multiset (SQL `... ALL`) semantics:
duplicates are preserved, and counts combine accordingly (sum, min,
truncated difference). All three require both inputs to share a TupleDesc.

The `range-*` and `multirange-*` variants are for temporal tables where
the range/multirange says when the tuple was true.

I also provide TQuel-style operators (`tquel-*`), based on Snodgrass's paper.
Here each *attribute* has a valid-time, instead of the overall tuple.
It is striking to me that he used a model so similar to the one-table-per-attribute structure that both
[Date/Darwen/Lorentzos](https://www.amazon.com/Time-Relational-Theory-Databases-Management/dp/0128006315)
and [Johnston](https://www.amazon.com/Bitemporal-Data-Practice-Tom-Johnston-ebook/dp/B00N9YPWD4/)
propose ("Sixth Normal Form" in D/D/L).
I think this is over-normalization, and at least Johnston hints that it is maybe going too far.
Also note that the TQuel valid-times are full *sets* of all valid-times, even with gaps.
They aren't limited to intervals.
I model that here with multiranges.

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

;; Range join: like join, but also requires the named valid-time range
;; attribute (a pair (s . e)) to overlap on both sides. Result has an extra
;; column with that name set to the intersection range.
(range-join pred 'valid-at left right)

;; Range cartesian product: every pair of tuples whose valid-time ranges
;; overlap. Same desc shape as range-join.
(range-cartesian-product 'valid-at left right)

;; Range select: an alias for `select`. Filters rows by pred; valid-time
;; columns are passed through unchanged.
(range-select pred r)

;; Range except: subtract overlapping ranges in r2 from rows in r1 that
;; agree on every other field. Range splits can produce multiple output rows.
(range-except 'valid-at r1 r2)

;; Full outer join. Use #:side 'left or #:side 'right for one-sided variants.
(outer-join pred employees depts #:side 'full)

;; Pretty-print a Rel as an ASCII table (nulls show as NULL).
(print-rel employees)
```

Nulls are written as `'()`. Padding from an outer join produces `'()`
in the columns of the unmatched side.

## Running the tests

The tests live in the `tests/` folder, split by category. From this
directory, run the whole suite with:

```
$ racket tests/all.rkt
```

The script exits 0 on success, 1 on any failure. You can also run a single
category (e.g. `racket tests/relops-tests.rkt`) or the whole folder via raco:

```
$ raco test tests/
```
