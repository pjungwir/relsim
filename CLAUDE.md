# relsim — notes for Claude

A Racket library implementing a small relational algebra. SQL semantics:
`'()` represents NULL, and Rels preserve duplicate tuples.

## Layout

The library is split across several files, each exporting one family of
definitions. `relsim.rkt` is the umbrella — it requires the others and
re-provides everything, so users just `(require "relsim.rkt")`.

- `relsim.rkt` — top-level entry point; pulls everything together via
  `all-from-out`. Holds no definitions of its own.
- `core.rkt` — basic data types (`tuple`, `tuple-desc`, `rel`), the shared
  private helpers (`field-index`, `list-remove`, `concat-desc`,
  `concat-tuples`, `tuple-ref`), and `print-rel` for ASCII-table output.
- `ranges.rkt` — range helpers (`range-overlaps`, `range-intersection`,
  `range-subtract`, `range-subtract-many`). Pure interval math, no deps.
- `multiranges.rkt` — multirange helpers (`multirange-canonical`,
  `multirange-union`, `multirange-intersection`, etc.). Requires `ranges.rkt`.
- `relops.rkt` — ordinary relational operators (`select`, `project`,
  `cartesian-product`, `join`, `semijoin`, `antijoin`, `union`, `intersect`,
  `except`, `outer-join`, `division`).
- `range-relops.rkt` — range-based temporal operators (`range-join`,
  `range-cartesian-product` and variants, `range-select`,
  `range-except`, `range-division`).
- `multirange-relops.rkt` — multirange-based temporal operators
  (`multirange-join`, `multirange-cartesian-product` and variants,
  `multirange-select`, `multirange-except`, `multirange-division`); mirrors `range-relops.rkt` but
  the valid-attr is a multirange. Requires `multiranges.rkt`.
- `tquel-relops.rkt` — TQuel operators, where each *attribute* carries its
  own valid-time multirange (`tsattr`, `rel->tquel`, `tquel->rel`,
  `tquel-*`). Requires `multiranges.rkt`.
- `tests/` — RackUnit tests, one file per family (`core-tests.rkt`,
  `ranges-tests.rkt`, `multiranges-tests.rkt`, `relops-tests.rkt`,
  `range-relops-tests.rkt`, `multirange-relops-tests.rkt`,
  `tquel-relops-tests.rkt`). Each provides its suite and is runnable on its
  own; `tests/all.rkt` aggregates them. Run with `racket tests/all.rkt`
  (exits 0/1) or `raco test tests/`.
- `identities/` — runnable demonstrations that the classical identity
  `Q × (R − S) = (Q × R) − (Q × S)` does *not* survive temporalization:
  `range-relops.rkt` (the four range variants; `/drop-old` is the one that
  holds), `multirange-relops.rkt` (same four variants over multiranges,
  same outcome), and `tquel-relops.rkt` (TQuel operators). `identities/probe/`
  holds the deeper stress-tests: `harness.rkt` runs any LHS/RHS identity over
  hand-picked cases plus random fuzz, and `ranges.rkt`, `multiranges.rkt`,
  `tquel.rkt` use it to probe the `/drop-old` distribution (holds for
  ranges/multiranges, fails for TQuel) and whether select distributes over
  the product. `ranges.rkt` also carries the proof sketch.
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
- `range-join` requires a field present in both descs; its output desc
  is `left ++ right ++ (list valid-attr)` — the same name appears on the
  two input columns and on the appended intersection column. Lookups by
  that name with `tuple-ref` will hit the leftmost occurrence; reach the
  intersection via position if needed.
- `outer-join` defaults to `#:side 'full`; `'left` and `'right` are also
  supported. Padding uses `'()` for every column on the missing side.
- Nulls are `'()`. There's a `null?-rel` helper that just calls `null?` —
  prefer it in user code so the intent is explicit.

## Conventions

- Keep the library dependency-free beyond `racket` and `rackunit`.
- Add new definitions to the file for their family (see Layout), and have
  `relsim.rkt` re-provide the file if it's a new module. Within a module,
  require only what it needs (`core.rkt`, `ranges.rkt`, etc.) rather than
  the `relsim.rkt` umbrella, to avoid a require cycle.
- Prefer adding a test in the matching `tests/*-tests.rkt` for any new
  operator or semantic change, and register it in the file's suite (which
  `tests/all.rkt` already aggregates). Each suite is small enough to read
  top-to-bottom.
- Write each `test-case` description as a verb-first indicative clause that
  reads as a sentence with an implicit "it" subject naming the thing under
  test: "keeps only requested fields", "returns #f for disjoint ranges",
  "drops a row when every kept attribute has an empty valid-at". Avoid bare
  noun labels ("union", "row count"), passive phrasings ("values are stored
  in order"), and restating the operator name ("select preserves
  duplicates"); the suite name already supplies the subject.
