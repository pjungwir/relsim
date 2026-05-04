# relsim — notes for Claude

A Racket library implementing a small relational algebra. SQL semantics:
`'()` represents NULL, and Rels preserve duplicate tuples.

## Layout

- `relsim.rkt` — the library. Defines `tuple`, `tuple-desc`, `rel`, the
  operators (`select`, `project`, `cartesian-product`, `join`, `semijoin`,
  `antijoin`, `union`, `intersect`, `except`, `outer-join`), and
  `print-rel` for ASCII-table output.
- `tests.rkt` — RackUnit tests. Run with `racket tests.rkt` (exits 0/1) or
  `raco test tests.rkt`.
- `README.md` — REPL examples.

## Design notes

- `tuple` is variadic and constructs a `tuple-internal` struct holding a
  list of values. We hide the raw struct constructor behind a variadic
  wrapper so users write `(tuple 1 2 3)` rather than `(tuple-internal '(1 2 3))`.
- `tuple-desc` is just a wrapped list of field-name symbols. Field lookups
  (`tuple-ref`, `project`, etc.) go through `field-index`, which errors on
  unknown names.
- `cartesian-product`, `join`, and `outer-join` all produce a Rel whose
  desc is the concatenation of the two input descs (duplicate field names
  are allowed and not renamed — callers disambiguate via position if
  needed).
- `union`, `intersect`, and `except` require `equal?` TupleDescs.
  `union` is bag union (just `append`); `intersect` and `except` use
  multiset semantics (SQL `INTERSECT ALL` / `EXCEPT ALL`) implemented via
  a counts hashtable keyed on tuples (transparent structs hash by value).
  Result preserves the row order of the left input.
- `semijoin` and `antijoin` keep the left desc and use `ormap` over the
  right rel's rows. They short-circuit on first match.
- `outer-join` defaults to `#:side 'full`; `'left` and `'right` are also
  supported. Padding uses `'()` for every column on the missing side.
- Nulls are `'()`. There's a `null?-rel` helper that just calls `null?` —
  prefer it in user code so the intent is explicit.

## Conventions

- Keep the library dependency-free beyond `racket` and `rackunit`.
- Prefer adding a test in `tests.rkt` for any new operator or semantic
  change. The suite is small enough to read top-to-bottom.
