#lang racket

;; relsim - a small relational algebra. This module is the top-level entry
;; point: it pulls together the pieces defined across the other files and
;; re-exports their public bindings, so users can `(require "relsim.rkt")` and
;; get everything. SQL semantics: '() represents NULL and Rels preserve
;; duplicate tuples.
;;
;; Layout:
;;   core.rkt          - tuple/rel data types, helpers, print-rel
;;   ranges.rkt        - range helpers
;;   multiranges.rkt   - multirange helpers
;;   relops.rkt        - ordinary relational operators
;;   range-relops.rkt  - range-based temporal operators
;;   tquel-relops.rkt  - TQuel (per-attribute, multirange) operators

(require "core.rkt"
         "ranges.rkt"
         "multiranges.rkt"
         "relops.rkt"
         "range-relops.rkt"
         "tquel-relops.rkt")

(provide (all-from-out "core.rkt")
         (all-from-out "ranges.rkt")
         (all-from-out "multiranges.rkt")
         (all-from-out "relops.rkt")
         (all-from-out "range-relops.rkt")
         (all-from-out "tquel-relops.rkt"))
