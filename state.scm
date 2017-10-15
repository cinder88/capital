(define-module (capital state)
  #:use-module (oop goops)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (capital base)
  #:export (obj->sexp save ledger-load)
  #:duplicates (merge-generics))

(define (date->sexp d)
  `((@ (srfi srfi-19) make-date)
	,(date-nanosecond d)
	,(date-second d)
	,(date-minute d)
	,(date-hour d)
	,(date-day d)
	,(date-month d)
	,(date-year d)
	,(date-zone-offset d)))

(define-method (obj->sexp x)
  (cond ((date? x)
		 (date->sexp x))
		((pair? x)
		 (cons 'list (map obj->sexp x)))
		((symbol? x)
		 `(quote ,x))
		((number? x)
		 x)
		((null? x)
		 '())
		(else (raise 'cannot-convert-to-sexp))))

(define-method (obj->sexp (c <capital>))
  `(make <capital>
	 #:scalar ,(obj->sexp (scalar c))
	 #:unit ,(obj->sexp (unit c))))

(define-method (obj->sexp (a <asset>))
  `(make <asset>
	 #:capital ,(obj->sexp (capital a))))

(define-method (obj->sexp (t <transaction>))
  `(make <transaction>
	 #:date ,(obj->sexp (date t))
	 #:from-account ,(obj->sexp (from-account t))
	 #:to-account ,(obj->sexp (to-account t))
	 #:asset ,(obj->sexp (asset t))))

(define-method (obj->sexp (l <ledger>))
  `(make <ledger>
	 #:accounts ,(obj->sexp (accounts l))
	 #:transactions ,(obj->sexp (transactions l))))

(define-method (save (l <ledger>) filename)
  (call-with-output-file filename
	(λ (f) (pretty-print (obj->sexp l) f))))

(define-method (ledger-load filename)
  (load filename))
