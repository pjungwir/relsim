# Temporal relational-algebra identities

## The question

A query planner (Postgres included) rewrites a query by applying
relational-algebra *equivalence rules*: `σ` pushed below a join, a Cartesian
product re-associated, a difference distributed, and so on. Those rewrites are
only legal because the rules are theorems of the ordinary relational algebra.

This library implements several *temporal* relational operators, and the point
of this folder is to ask: **do the temporal operators obey the same
identities?** Where they don't, a planner that blindly applies the classical
rule to a temporal query would return wrong answers.

## The operator families

We model a tuple's valid-time three different ways, each with its own operator
family (see the top-level `../CLAUDE.md` for the code layout):

- **`range-*`** (`../range-relops.rkt`): the valid-time is a single range
  `(s . e)`.
- **`multirange-*`** (`../multirange-relops.rkt`): the valid-time is a
  multirange (a set of ranges).
- **`tquel-*`** (`../tquel-relops.rkt`): Snodgrass's TQuel model, where each
  *attribute* carries its own valid-time multirange and the product is plain
  attribute concatenation.

## What's here

- `EquivalenceRules.pdf` is the list of 12 classical rules we test against. It
  is the attachment to [this pgsql mailing-list message][thread] (Ali Piroozi
  asking which equivalence rules Postgres implements); the rules themselves are
  the standard set from Silberschatz/Korth/Sudarshan.
- `range-relops.rkt`, `multirange-relops.rkt`, `tquel-relops.rkt` are runnable
  demonstrations of one specific identity (distributivity of the product over
  difference) per family.
- `probe/` holds the stress-tests. `probe/harness.rkt` runs any identity
  (given as an LHS and RHS expression) over hand-picked cases plus thousands of
  random fuzz trials. `probe/ranges.rkt`, `probe/multiranges.rkt`, and
  `probe/tquel.rkt` probe the `/drop-old` distribution and select-over-product;
  `probe/equivalences.rkt` probes the PDF rules. The table below is generated
  by reading those probes' output ("fuzz: 0/N failures" means it held).

[thread]: https://www.postgresql.org/message-id/CAMi-Eo1Wrxft=0ZsvKkZRYkoFVGRnbhbjL6a=rUf58xOGuj7DA@mail.gmail.com

## The 12 classical rules, and whether they survive

Legend: **yes** holds on every trial; **NO** fails (counterexamples found);
**reorder** holds only up to column reordering (an artifact of our positional
column model, not a temporal effect); **n/a** that operator combination isn't
defined in this library. A * marks results checked empirically by a fuzz probe;
the rest are by inspection (the operator is literally the ordinary one, so the
ordinary proof carries over).

| # | Rule | range | multirange | tquel |
|---|------|:-----:|:----------:|:-----:|
| 1 | `σ` cascade: `σ_{p∧q}(E) = σ_p(σ_q(E))` | yes | yes | yes |
| 2 | `σ` commutes: `σ_p(σ_q(E)) = σ_q(σ_p(E))` | yes | yes | yes |
| 3 | `Π` cascade: outer projection wins | yes | yes | yes |
| 4 | `σ_θ(E1 × E2) = E1 ⋈_θ E2` | yes | yes | n/a |
| 5 | join commutes: `E1 ⋈ E2 = E2 ⋈ E1` | reorder* | reorder | reorder |
| 6 | join/product associates | **NO*** | **NO*** | yes* |
| 7 | `σ` distributes over the join | yes* | yes | n/a |
| 8 | `Π` distributes over the join | n/a | n/a | n/a |
| 9 | `∪`, `∩` commute | yes | yes | yes |
| 10 | `∪`, `∩` associate | yes | yes | yes |
| 11 | `σ` distributes over `−` | yes* | yes* | yes* |
| 12 | `Π` distributes over `∪` | yes | yes | yes |

Notes on the non-obvious cells:

- **Rule 4 / 7 (tquel = n/a).** TQuel has no separate theta-join; its product
  is plain Cartesian product and a join is just `σ` over that product, so the
  rules are trivially true but there's nothing distinct to test.
- **Rule 5 (reorder).** `range-cartesian-product Q R` and `... R Q` have the
  same rows but their columns (and the appended valid-at) come out in the other
  order, so they're never `equal?` on the nose. This is exactly how plain
  Cartesian product behaves in any positional model; it is not a temporal
  effect.
- **Rule 8 (n/a).** We have no projection that understands valid-time (only the
  ordinary `project`), so the temporal version of "push projection through a
  join" isn't modeled.
- **Rule 9/10 (set ops).** `union` is bag append and `intersect`/`except` use
  multiset counts, so commutativity/associativity hold *as bags* (our probes
  compare bag-equal). `tquel-union` merges by value and so is also order-free.

## The identity that fails for ranges: associativity (Rule 6)

This is the answer to "find an identity that does not hold for ranges." The
plain `range-cartesian-product` is **not associative**. With
`Q = {(q1, [0,30))}`, `R = {(r1, [10,40))}`, `S = {(s1, [20,50))}`:

```
(Q × R) × S   ...  s1 | (20 . 50) | (20 . 30)     <- last valid-at = Q ∩ S
Q × (R × S)   ...  s1 | (20 . 50) | (10 . 30)     <- last valid-at = Q ∩ R
```

The product appends a fresh `valid-at` column holding the pair's intersection,
but it leaves the inputs' original `valid-at` columns in place, and the *next*
product looks up the **leftmost** `valid-at` (Q's original). So `(Q × R) × S`
intersects Q's time with S and forgets R, while `Q × (R × S)` intersects Q with
R and forgets S. Neither equals the intended `Q ∩ R ∩ S`, and the two
disagree. The fuzzer finds a mismatch in roughly a third of random trials
(`probe/equivalences.rkt`).

Multirange behaves identically (same appended-column issue). TQuel does **not**
have this problem: its product is plain concatenation with the time living in
the attributes, so it associates like classical relational algebra.

### The `/drop-old` fix

`range-cartesian-product/drop-old` collapses each pair to the single
intersection `qt ∩ rt`, dropping the stray source-time columns. With no
leftover `valid-at` to misread, **associativity is restored** (0 failures over
3000 trials for both range and multirange). This is the same fix that restores
the next identity.

## Extra identities we studied (not in the PDF)

| Identity | range | multirange | tquel |
|----------|:-----:|:----------:|:-----:|
| product distributes over `−`: `Q × (R − S) = (Q × R) − (Q × S)` | **NO** (`/drop-old`: yes) | **NO** (`/drop-old`: yes) | **NO** |
| `σ` (left-only predicate) distributes over the product | yes | yes | yes |

The product-over-difference failure is the original motivating example
(`range-relops.rkt`, `multirange-relops.rkt`, `tquel-relops.rkt`, and
`probe/ranges.rkt`'s proof sketch). For ranges and multiranges the *plain*
product fails and `/drop-old` fixes it, for the same reason as associativity:
leftover valid-at columns mismatch in the outer difference's key. TQuel fails
for a different reason: its per-attribute difference over-subtracts, cancelling
an attribute's whole valid-time even when the tuple combination only co-occurs
over a smaller window.

## A note on Dignös's "select doesn't distribute"

Anton Dignös points out (PGConf [slide][dignos]) that selection on the *input
rows' valid-time* does not distribute the way one might hope. That failure is
about predicates over the valid-time attribute itself. Our operators drop the
input valid-times (`/drop-old`, and TQuel keeps time inside attributes), and the
`σ` predicates we test are over ordinary attributes, never the valid-time
column. So that particular failure mode isn't reachable in what we model here;
there is nothing for us to test against it.

[dignos]: https://illuminatedcomputing.com/talks/pgconf2026-temporal-roadmap/index.html#/26

## Takeaway

Most classical rules survive temporalization: the selection rules, the set-op
commutativity/associativity, and selection-over-difference all hold for every
family. What breaks is the **Cartesian product's bookkeeping of valid-time**:
the plain `range`/`multirange` products are neither associative nor
distributive over difference, because they retain the inputs' valid-times as
ordinary columns. Collapsing each result to the single intersection
(`/drop-old`) repairs both. TQuel keeps the product well-behaved (time lives in
the attributes) but trades that for a difference operator that over-subtracts.
