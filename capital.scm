(use-modules (oop goops))

(define %units '())
(define %transactions '())

(define-class <unit> ()
  (symbol #:init-keyword #:symbol))

(define-class <capital> ()
  (scalar #:init-keyword #:scalar
		  #:accessor scalar)
  (unit #:init-keyword #:unit
		#:accessor unit))

(define-class <transaction> ()
  (from-account #:init-keyword #:from-account
				#:accessor from-account)
  (to-account #:init-keyword #:to-account
			  #:accessor to-account)
  (capital #:init-keyword #:capital
		   #:accessor capital))

(define (new-unit symbol)
  (set! %units
		(cons (make <unit> #:symbol symbol)
			  %units)))

(new-unit '$)
(new-unit '*$)

(define (new-transaction from to capital)
  (set! %transactions
		(cons (make <transaction>
				#:from-account from
				#:to-account to
				#:capital capital)
			  %transactions)))
					
					
