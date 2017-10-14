(add-to-load-path (dirname (getcwd)))

(use-modules (oop goops)
			 (capital base)
			 (capital arithmetic)
			 (capital save))

(define a1 'assets)
(define a2 'expense)

(define c1 (make <capital> #:scalar 5 #:unit 'USD))
(define c2 (make <capital> #:scalar 7 #:unit 'USD))
(define c3 (make <capital> #:scalar 3 #:unit 'USD-V))

(define t1 (make <transaction>
			 #:date (date 2017 09 01)
			 #:from-account a1
			 #:to-account a2
			 #:asset c1))

(define t2 (make <transaction>
			 #:date (date 2017 09 01)
			 #:from-account a1
			 #:to-account a2
			 #:asset c2))

(define t3 (make <transaction>
			 #:date (date 2017 09 01)
			 #:from-account a1
			 #:to-account a2
			 #:asset c3))

(define t4 (make <transaction>
			 #:date (date 2017 09 01)
			 #:from-account a1
			 #:to-account a2
			 #:asset c3))

(define L (make <ledger>))

(map (λ (t) (+! L t)) (list t1 t2 t3 t4))
