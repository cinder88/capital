(define-module (capital usd)
  #:use-module (capital base)
  #:use-module (oop goops)
  #:export (USD USD-V)
  #:duplicates (merge-generics))

(define USD (make <unit> #:name 'USD))
(define USD-V (make <unit> #:name 'USD-V))
