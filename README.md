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

Operators provided: `select`, `project`, `cartesian-product`, `join`, `range-join`, `range-cartesian-product`, `range-select`, `range-except`, `range-division`, `multirange-join`, `multirange-cartesian-product`, `multirange-select`, `multirange-except`, `multirange-division`, `semijoin`, `antijoin`, `union`, `intersect`, `except`, `outer-join`, `division`, `small-divide`.

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

## Notes

### Why doesn't anyone talk about relational division?

As long as we're thinking about relational algebra, I've often wondered why the
division operator gets so little attention. Claude said:

A few reasons converge:

- **It's derived, not primitive.** Codd's algebra is complete with six
  primitives (σ, π, ×, ∪, −, rename); division is expressible in terms of
  them:

$$
R \div S = \pi_K(R) - \pi_K\big((\pi_K(R) \times S) - R\big)
$$

  (K is R's columns minus S's). So it carries no theoretical weight: it's a
  convenience for one query shape, and textbooks file it under "derived
  operations" next to intersection.
- **No SQL keyword.** Division answers universal-quantification queries
  ("suppliers who supply *every* part"). SQL never got a `DIVIDE`, so people
  write those as double-`NOT EXISTS`, or `GROUP BY ... HAVING COUNT(...) =
  (SELECT COUNT(*) ...)`, and never reach for "division" as a concept.
- **The "for all" shape is rarer.** Existence queries (joins, semijoins)
  dominate real workloads; genuine "for all of this set" questions are
  uncommon.
- **It's finicky.** The empty-divisor case is notoriously counterintuitive:
  `R ÷ {}` returns *all* of `π_K(R)`, because every key vacuously pairs with
  every member of the empty set.

But "no one" is too strong, and the exception is the relational-theory crowd
this project keeps citing: Date and Darwen discuss it at length, mostly to
point out that Codd's definition is deficient and to propose replacements
(Todd's division, their "Small Divide" / "Great Divide").

So relsim now includes `division`, plus temporal `range-division` and
`multirange-division`. (Snodgrass doesn't give a TQuel version, so neither do
we.) The temporal versions use sequenced semantics: a key is valid at an
instant exactly when, for every divisor value valid then, the combined tuple
is in the dividend then. As you'd expect for a "for all, over time" operator,
the result valid-time is bounded to the divisor's lifespan, so the temporal
empty-divisor case yields nothing instead of everything.

And (speaking myself here), just a few weeks ago I happened to read
["Temporal Aggregates and Temporal Universal Quantification in Standard SQL" by Esteban Zimányi](https://sigmodrecord.org/publications/sigmodRecord/0606/p16-article-zimanyi.pdf).
So that is actually about temporal division! Claude again:

His query is "employees who work on *all* projects controlled by their
department," realized in standard SQL with the double-`NOT EXISTS` idiom. His
recipe is three steps: find the periods over which the inputs are constant,
compute the universal quantification per period, then coalesce. Our
`range-division` / `multirange-division` do all three internally with multirange
arithmetic, in one operator.

His examples are schematic timeline diagrams (a ✓/✗ result row over relative
periods), split into four cases by which of `WorksOn`, `Affiliation`, and
`Controls` are temporal. The general case, with `Controls` and `WorksOn` both
temporal, is his Case 2, and it maps exactly onto ours: a worker qualifies
precisely while it works on every project its department *currently* controls.
That is the regression test in `tests/range-relops-tests.rkt` (and its
multirange twin). We even agree on his subtle Case 4 point ("the end of a
`WorksOn` row induces no result period, since then the project is not
controlled"): our `⋃_s (S_s − R_{k,s})` only counts a missing project against a
worker *while that project is required*.

The differences are about packaging. His is plain SQL: a stack of views plus
`NOT EXISTS`, which he himself flags as "extremely inefficient"; ours is a
single operator. And he threads `Affiliation` and `Controls` together to derive
each employee's required-project set, whereas our operator takes the
already-formed temporal divisor `S`. So reproducing his cases is a matter of how
you build `S`'s valid-times: a non-temporal divisor gets forever-periods, and a
temporal affiliation is folded into the divisor.

### Alternatives to division

Codd's `DIVIDE` is the operator everyone wants to replace, and relsim's
`division` is that original. The lineage of replacements is mostly Date and
Darwen:

- **Codd's division** is deficient: the empty-divisor anomaly (`a ÷ {}` is
  *everything*), awkward heading arithmetic (the result heading is the
  dividend's minus the divisor's), and it conflates "who are the candidates"
  with "what is the relationship."
- **Todd's division** generalizes it so the relationship relation is given
  explicitly rather than implied.
- **The Small Divide**, written `A DIVIDEBY B PER C`: `A` is the candidate set
  `{X}`, `B` the required set `{Y}`, `C` the relationship `{X,Y}`, and the
  result is `{ x ∈ A : ∀ y ∈ B, ⟨x,y⟩ ∈ C }`. Separating the candidates `A`
  from the relationship `C` removes the heading subtraction, and the empty-`B`
  case gives a well-defined "all of `A`".
- **The Great Divide** is a more symmetric form for a double universal
  quantification (its exact signature is worth checking in the book).
- **Date's actual recommendation** is to skip a divide operator entirely and
  use *image relations* with relational comparison. The image relation of a
  tuple is the set of values related to it, so "supplies every part" is just
  `image ⊇ P`; in Tutorial D, roughly `S WHERE (!!SP){PNO} ⊇ P{PNO}`. This
  states the "for all" directly and handles the edge cases cleanly, which is
  why he argues a dedicated `DIVIDE` is largely unnecessary.

Two notes for this project. relsim now provides the Small Divide as `small-divide` (candidates `DIVIDEBY` divisor `PER` the relationship); dividing by an empty divisor returns every candidate, even ones the relationship never mentions. And the
image-relation-with-`⊇` formulation is exactly the sequenced temporal division
above: "at each instant the image contains the required set" is what
`range-division` and `multirange-division` compute.
