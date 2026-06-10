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

The plain product appends that fresh `valid-at` column but leaves the inputs'
original `valid-at` columns in place. With three columns now named `valid-at`,
the next product reads the **leftmost** one (Q's original), so `(Q × R) × S`
intersects Q with S and `Q × (R × S)` intersects Q with R. That is a vivid
symptom, and the fuzzer catches it in roughly a third of random trials
(`probe/equivalences.rkt`), but it is **not the root cause**: it is just what
the duplicated column name happens to do.

The `/rename-old` variant exposes the real cause. It renames each input's
`valid-at` to `old-valid-at`, so every column is uniquely named and the product
reads the *correct* running intersection. Both sides then agree on the final
`valid-at` (`[20,30) = Q ∩ R ∩ S`). The identity *still* fails, because the
retained `old-valid-at` columns differ: one side kept `Q ∩ R = [10,30)`, the
other kept `R ∩ S = [20,40)`. The answer is identical; only the leftover
bookkeeping differs.

So the real cause is that **valid-time is not an attribute but a qualifier of
the other attributes**: it records *when* a fact holds, not *which* fact it is.
Tuple equality, and the set-difference operator's "match on every column" rule,
nonetheless treat the retained input valid-times as part of a row's identity, so
the same fact carrying different leftover timestamps reads as a different row and
the algebra breaks. Keeping the times as columns is the problem; the leftmost
lookup is just one way the plain variant stumbles into it.

Multirange behaves identically. TQuel does **not** have this problem: its
product is plain concatenation with the time living *inside* each attribute, so
there is no free-standing valid-time column to compare on, and it associates
like classical relational algebra.

### The `/drop-old` fix

`range-cartesian-product/drop-old` collapses each pair to the single
intersection `qt ∩ rt` and drops the inputs' valid-time columns. With the
qualifier-columns gone, equality and difference compare only on the genuine
attributes, and **associativity is restored** (0 failures over 3000 trials for
both range and multirange). This is the same fix that restores the next
identity.

I find some ironies here.
First, the TSQL2 standard was [criticized by Date and Darwen](https://www.dcs.warwick.ac.uk/~hugh/TTM/OnTSQL2.pdf)
and they have similar criticisms of SQL:2011 in [their book with Lorentzos](https://www.amazon.com/Time-Relational-Theory-Databases-Management/dp/0128006315).
They don't like how `PERIOD`s are not part of the relational math, but weird table metadata.
And practically speaking, I agree this is annoying: they compose poorly.
You can't `SELECT` them, or get them from a view, or pass them to a function, or return them from a function, or use them in aggregates or window functions or to define groups, etc.
The standard defines a few special-case predicates you can put in `WHERE` clauses with magic syntax.
But from what I've seen [no RDBMS implements them for application-time](https://illuminatedcomputing.com/posts/2019/08/sql2011-survey/) (understandably, since it's annoying).
That's why I like using Postgres rangetype columns.
But treating them like attributes spoils the algebra.
And that isn't surprising: semantically, they aren't attributes, but qualifiers of the other attributes.
So temporal operators need to treat them differently.

Note that `PERIOD`s don't improve things though.
You still have real columns for the start and end times,
and carrying those forward is just as bad.
So all their other disadvantages don't buy you anything algebraically.

And to be fair to Date/Darwen/Lorentzos,
they also treat valid-time as more than a regular attribute.
Their temporal `U_*` operators are defined in terms of `PACK` and `UNPACK`,
which effectively drop the input valid-times from their results.

One thing that `PERIOD`s give you is table metadata to identify which column is special.
SQL:2011 forbids more than one application-time `PERIOD` on a table.
But I think that restriction is too limiting,
and a join/union/etc could just let you name the valid-time columns involved.
After all, you're already typically naming the other columns in your join condition.
Relsim models that approach: every operator takes `'valid-at` as a parameter
rather than relying on table metadata.

The second irony is that one significant result in the
[Dignös/Böhlen/Gamper "Temporal Alignment" paper](https://www.zora.uzh.ch/entities/publication/5c71ee3a-f8f4-4d5d-a9fb-3b85cde08e89)
is precisely to *keep* the input valid-times (via their "extend" operator).
Sometimes you want them.
For instance imagine a database for renting vacation homes, where you give a discount based on the length of your stay. Then when you join reservations to discounts, it's based on the length of the valid-time.
Or if you run an aggregate query, you have to cut up the input valid-times into smaller slices so that every record in the group aligns.
If you are aggregating something numeric like a budget, you often want to scale it proportionally to the slice's new valid-time length.
But really those use-cases only require the input valid-times for the intermediate calculations.
It still makes sense to drop them from the result.

## Extra identities we studied (not in the PDF)

| Identity | range | multirange | tquel |
|----------|:-----:|:----------:|:-----:|
| product distributes over `−`: `Q × (R − S) = (Q × R) − (Q × S)` | **NO** (`/drop-old`: yes) | **NO** (`/drop-old`: yes) | **NO** |
| `σ` (left-only predicate) distributes over the product | yes | yes | yes |

The product-over-difference failure is the original motivating example
(`range-relops.rkt`, `multirange-relops.rkt`, `tquel-relops.rkt`, and
`probe/ranges.rkt`'s proof sketch). For ranges and multiranges the *plain*
product fails and `/drop-old` fixes it, for the same reason as associativity:
the retained input valid-times are compared as if they were attributes (here in
the outer difference's match key, where R's leftover time and S's leftover time
differ, so nothing cancels). TQuel fails for a different reason: its
per-attribute difference over-subtracts, cancelling an attribute's whole
valid-time even when the tuple combination only co-occurs over a smaller window.

## A note on Dignös's "select doesn't distribute"

Anton Dignös points out (PGConf [slide][dignos]) that selection on the *input
rows' valid-time* does not distribute the way one might hope. That failure is
about predicates over the valid-time attribute itself. Our operators drop the
input valid-times (`/drop-old`, and TQuel keeps time inside attributes), and the
`σ` predicates we test are over ordinary attributes, never the valid-time
column. So that particular failure mode isn't reachable in what we model here;
there is nothing for us to test against it.

[dignos]: https://illuminatedcomputing.com/talks/pgconf2026-temporal-roadmap/index.html#/26

## Do multiranges satisfy any identity that ranges don't?

Assuming we always drop the old valid-times (so the product is the
well-behaved `/drop-old` form): **no.** Every identity that holds for
multiranges also holds for ranges. If anything the implication runs only one
way, so ranges could satisfy *more* identities, not fewer.

The reason is that **ranges are the "expanded" image of multiranges.** Define a
map `φ` that takes a multirange relation and splits each tuple whose valid-time
is `{[a,b), [c,d), ...}` into one gapless tuple per piece. On range-shaped
inputs (every valid-time a single range), each operator commutes with `φ`:

- `range-select = select = φ ∘ multirange-select` (select never touches the
  valid-time).
- `range-except(R, S) = φ(multirange-except(R, S))`: `multirange-except` carries
  the leftover as one multirange, and `range-except` emits exactly that
  multirange's canonical gapless pieces as separate rows.
- `range-cartesian-product/drop-old(Q, X) = φ(multirange-…/drop-old(Q, X))`:
  intersection distributes over the piece-decomposition, and `/drop-old`
  collapses each pair to a single intersection, so no stray valid-at column
  survives to be re-read.

So `φ` is a homomorphism from the multirange algebra onto the range algebra.
Apply it to both sides of any equation: if `LHS = RHS` for all multirange
inputs, then `LHS = RHS` for all range inputs too. The multirange model differs
only in *representation* (one tuple with a gappy valid-time vs several gapless
tuples), not in its equational theory. The probes agree: nothing in
`probe/equivalences.rkt` separated the two families.

The interesting caveat is the *opposite* direction. `φ` is surjective but not
injective: `{(q1, {[0,5), [10,15)})}` and `{(q1, {[0,5)}), (q1, {[10,15)})}`
expand to the same range relation. So range-equality does **not** imply
multirange-equality, which leaves room for identities that hold for **ranges
but not multiranges**: ones sensitive to whether a gappy fact is stored as one
tuple or several (anything that counts value-equivalent tuples). That is the
duplicate-elimination / coalescing distinction the temporal-element literature
cares about, and it is where a real separation would live.

Note that if we "coalesce" (using Snodgrass's meaning), then `φ` becomes
bijective, and we have an isomorphism. Consider "value-equivalent" tuples:
tuples whose attributes are all equal, ignoring their valid times. Then for
ranges, coalesce means combining value-equivalent tuples with adjacent/overlapping
ranges into one. It is a kind of canonicalization. Multiranges are similar, but
they let us combine *all* value-equivalent tuples into one multirange (since we
can handle gaps). In that case, there is no ambiguity mapping from a range-relation
to a multirange-relation. But this only works for relations with a temporal primary
key (forbidding equality on the key column(s) with overlaps on the valid time).
Otherwise we have a bag, not a set, and what does coalesce even mean then? For
instance what should `{(q1, [1,15)), (q1, [10,20))}` give us? After coalescing,
do we keep the fact that we saw `[10,15)` twice? And similarly for multiranges.
But coalescing is expensive and ignored by SQL:2011. It seems unlikely people
will use it. And how would the Postgres planner know you are doing it?

## Takeaway

Most classical rules survive temporalization: the selection rules, the set-op
commutativity/associativity, and selection-over-difference all hold for every
family. What breaks is the **Cartesian product's bookkeeping of valid-time**:
the plain `range`/`multirange` products are neither associative nor distributive
over difference, because they retain the inputs' valid-times as ordinary
columns, even though a valid-time is a *qualifier* of the other attributes, not
an identifying attribute. Tuple equality and set-difference then compare on
those leftover times and see the same fact as different rows. Collapsing each
result to the single intersection (`/drop-old`) drops the qualifier-columns and
repairs both. TQuel keeps the product well-behaved (time lives inside the
attributes) but trades that for a difference operator that over-subtracts.
