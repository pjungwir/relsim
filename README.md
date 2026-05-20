# relsim

A small Racket library for building relations and evaluating expressions
in the style of the relational algebra. SQL-flavored: nulls are allowed
(represented as `'()`) and duplicate tuples are preserved.

The real point of this is to experiment with *temporal* relational operators,
and see if they obey the same algebraic identities as their traditional analogues.
This matters for query transformations done by the planner.

According to "TQuel Overview" by Richard Snodgrass in *Temporal Databases: Theory,
Design, and Implementation* (1993), one identity that is not the same is that
Cartesian product does not distribute over difference. In other words, this
identity does not hold:

```
Q ⨯̂ (R −̂ S) ≡ (Q ⨯̂ R) −̂ (Q ⨯̂ S)
```

Since other joins can all be defined in terms of Cartesian product, I suspect
many transformations built into databases like Postgres will not be valid.

Is the problem because of adding a new `valid_at` column, to supplement the
inputs' columns? I tried two kinds of Cartesian product, one adding a new
`valid_at` column and another that replaces the inputs' columns. But you can
find counterexamples for both cases.

Actually, my `/drop-old` variant *does* hold. The issue isn't adding a valid-time column,
but *keeping* the old valid-time columns. In other words, treating those as if they were regular attributes.
Really they are qualifiers of the other attributes in the tuple.

There are two ironies here:
First, Date criticized TSQL2 and SQL:2011 for not representing application-time
within the formal relational mathematics.
`PERIOD`s are some weird thing that isn't an attribute.
This means they compose poorly:
you can't `SELECT` them, use them in an expression, output them from a `VIEW`,
pass them to a function or return them, `GROUP BY` them, etc.
Second, a significant part of Dignös's research is preserving the input valid-times
(his "extend" operator in "Temporal Alignment").
He showed this can be useful for scaling aggregate inputs when their rows get sliced up to align,
filtering tables by valid-time length, or joining tables by valid-time length,
e.g. giving deeper hotel reservation discounts the longer your stay.
(Actually those last two use-cases don't require the input valid-time to be in the *result*,
only the intermediate normalized tuples, so maybe they aren't applicable.)

But semantically, dropping the input valid-times is correct:
they weren't regular attributes.
Intuitively it makes sense for the algebra to work out here and not there.
So perhaps omitting them should be the default.
If the user wants them, he can `SELECT` them before doing a join (or whatever).
Also it makes sense that these really only mess things up for setops,
which compare on *all attributes*.
For common joins you wouldn't compare on these attributes.
Well you would, but with overlaps.
That seems sufficiently symmetrical and well-behaved that it probably doesn't cause problems.
But I still need to explore how selection distributes over Cartesian product and joins. . . .

After all this experimentation with tuple-timestamped operators, I double-checked Snodgrass's TQuel definition: it's just regular Cartesian product! My temporal Cartesian products included an implicit overlaps condition, and his doesn't. But here his data model is significant: in his system, every *attribute* gets its own valid-time, not the overall tuple. (This is doing the same work that Date and Johnston get to in their sixth-normal form structure.) Moreover, each valid-time is not an interval, but a set of all valid-times (which lets him avoid duplicate tuples wrt the other attributes). Actually we can represent such a set conveniently with multiranges.

So I added a separate `tquel.rkt` file with a `tsattr` struct, holding a value plus a multirange of valid-times,
and TQuel tuples just have a `tsattr` for each attribute.
I included a function to convert a regular tuple to a TQuel tuple (just copy the valid-time onto each attribute),
and another function to convert a TQuel tuple into one or more regular tuples.
He also mentions "homogeneous tuples", which is a TQuel tuple whose attributes all have the same valid-time set.
In that case it would convert to a single regular tuple.
I haven't done anything here with that idea yet, but his result about reducibility only applies to homogeneous tuples.

Then I made functions for each TQuel operator, and I put another counter-example in `identities.rkt`.
Here the problem isn't that we're retaining the input valid-times.
It's something else.
I haven't put my finger on it yet.

## Concepts

- **Tuple** — a struct holding a list of values. Construct variadically:
  `(tuple 1 "Alice" 10)`.
- **TupleDesc** — names the fields of the tuples in a Rel:
  `(tuple-desc '(id name dept-id))`.
- **Rel** — a TupleDesc plus a list of Tuples:
  `(rel desc (list (tuple ...) ...))`.

Operators provided: `select`, `project`, `cartesian-product`, `join`,
`temporal-join`, `temporal-cartesian-product`, `temporal-select`,
`temporal-except`, `semijoin`, `antijoin`, `union`, `intersect`, `except`,
`outer-join`.

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

;; Temporal join: like join, but also requires the named valid-time range
;; attribute (a pair (s . e)) to overlap on both sides. Result has an extra
;; column with that name set to the intersection range.
(temporal-join pred 'valid-at left right)

;; Temporal cartesian product: every pair of tuples whose valid-time ranges
;; overlap. Same desc shape as temporal-join.
(temporal-cartesian-product 'valid-at left right)

;; Temporal select: an alias for `select`. Filters rows by pred; valid-time
;; columns are passed through unchanged.
(temporal-select pred r)

;; Temporal except: subtract overlapping ranges in r2 from rows in r1 that
;; agree on every other field. Range splits can produce multiple output rows.
(temporal-except 'valid-at r1 r2)

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

# Notes

The reason cartesian product doesn't distribute over except is because of keeping the old valid-times and adding the new valid-time. That means that the *other* part of the tuples don't fully match (because there are more valid-times mixed in), so the except doesn't subtract anything. If cartesian product replaces *all* valid times, or removes all and adds its own, I think it would work.

Alternately, if cartesian product kept the full original tuples and added a new valid-time, then temporal except should do that too. Then I think we might have a match. Also I need to make sure to use the *right* valid-time attribute for the operators.

I think the /overwrite-old option is getting the closest, and it would be there 100% if it weren't keeping *both* columns. Better would be to drop those columns and then add on just one valid-at column at the end. Let's try that next.
Btw that suggests that the real problem isn't adding the new column, but keeping the old columns. And that makes sense, because those columns are not part of the asserted history, but metadata about the assertion. So carrying them forward doesn't make sense.

Indeed, the /drop-old option preserves the identity!
