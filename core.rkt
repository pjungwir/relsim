#lang racket

;; Core data types for relsim - the basic tuple/rel construction and
;; manipulation that every operator family builds on, plus `print-rel` for
;; ASCII-table output. SQL semantics: '() represents NULL and Rels preserve
;; duplicate tuples.

(provide (struct-out tuple-desc)
         (struct-out rel)
         tuple
         tuple?
         tuple-values
         make-tuple-from-list
         tuple-ref
         null?-rel
         field-index
         list-remove
         concat-desc
         concat-tuples
         print-rel)

;; ---------------------------------------------------------------------------
;; Core data types
;; ---------------------------------------------------------------------------

;; A Tuple is a struct holding a list of values aligned with a TupleDesc's
;; fields. The empty list '() represents SQL NULL.
;;
;; The constructor is variadic: (tuple 1 'x "hi") => tuple with values '(1 x "hi").
(struct tuple-internal (values) #:transparent
  #:constructor-name make-tuple-from-list
  #:reflection-name 'tuple)

(define (tuple . vs) (make-tuple-from-list vs))
(define tuple? tuple-internal?)
(define tuple-values tuple-internal-values)

;; A TupleDesc lists the field names (symbols) of a Rel's tuples.
(struct tuple-desc (fields) #:transparent)

;; A Rel is a TupleDesc plus a list of Tuples (duplicates allowed, like SQL).
(struct rel (desc tuples) #:transparent)

;; '() is our null marker.
(define (null?-rel v) (null? v))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;; Get a list, except for a given index
(define (list-remove lst i)
  (append (take lst i) (drop lst (+ i 1))))

;; Get the index of a field from a tuple-desc
(define (field-index desc field)
  (or (index-of (tuple-desc-fields desc) field)
      (error 'field-index "no such field: ~a" field)))

;; Get a field value from a tuple given the Rel's TupleDesc.
(define (tuple-ref t desc field)
  (list-ref (tuple-values t) (field-index desc field)))

(define (concat-desc d1 d2)
  (tuple-desc (append (tuple-desc-fields d1)
                      (tuple-desc-fields d2))))

(define (concat-tuples t1 t2)
  (make-tuple-from-list (append (tuple-values t1) (tuple-values t2))))

;; ---------------------------------------------------------------------------
;; Pretty-printing
;; ---------------------------------------------------------------------------

(define (cell->string v)
  (cond [(null? v) "NULL"]
        [(string? v) v]
        [else (format "~a" v)]))

;; Print r as an ASCII table with column headers from its TupleDesc.
(define (print-rel r [out (current-output-port)])
  (define headers (map symbol->string (tuple-desc-fields (rel-desc r))))
  (define rows (map (lambda (t) (map cell->string (tuple-values t)))
                    (rel-tuples r)))
  (define widths
    (for/list ([i (in-naturals)] [h (in-list headers)])
      (apply max (string-length h)
             (map (lambda (row) (string-length (list-ref row i))) rows))))
  (define sep
    (string-append "+"
                   (string-join
                    (for/list ([w (in-list widths)])
                      (make-string (+ w 2) #\-))
                    "+")
                   "+"))
  (define (row-line cells)
    (string-append "| "
                   (string-join
                    (for/list ([c (in-list cells)] [w (in-list widths)])
                      (~a c #:min-width w))
                    " | ")
                   " |"))
  (displayln sep out)
  (displayln (row-line headers) out)
  (displayln sep out)
  (for ([row (in-list rows)]) (displayln (row-line row) out))
  (displayln sep out))
