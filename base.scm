(define-module (capital base)
  #:use-module (oop goops)
  #:use-module (ice-9 r5rs)
  #:use-module (srfi srfi-19)
  #:export (<capital> scalar unit)
  #:export (<asset> capital)
  #:export (<transaction> date from-account to-account asset)
  #:export (<ledger> transactions accounts account-strict)
  #:export (date value +! balance)
  #:duplicates (merge-generics))

(define (date year month day)
  (make-date 0 0 0 0 year month date 0))

(define-class <capital> ()
  (scalar #:init-keyword #:scalar
		  #:accessor scalar)
  (unit #:init-form 'USD
	    #:init-keyword #:unit
		#:getter unit))

(define-method (equal? (c1 <capital>) (c2 <capital>))
  (and (equal? (scalar c1) (scalar c2))
	   (equal? (unit c1) (unit c2))))

(define-method (value (c <capital>))
  (list (scalar c) (unit c)))

(define-class <asset> ()
  (capital #:init-form '()
		   #:init-keyword #:capital
		   #:accessor capital))

(define-method (value (a <asset>))
  (map value (capital a)))

(define-class <transaction> ()
  (date #:init-keyword #:date
		#:accessor date)
  (from-account #:init-keyword #:from-account
				#:accessor from-account)
  (to-account #:init-keyword #:to-account
			  #:accessor to-account)
  (asset #:init-keyword #:asset
			  #:accessor asset))

(define-class <ledger> ()
  (transactions #:init-form '() #:accessor transactions)
  (accounts #:init-form '() #:accessor accounts))

(define-method (+! (L <ledger>) (acct <symbol>))
  (let ((A (accounts L)))
	(define (find-or-add account possible-matches)
	  (cond ((null? possible-matches) (cons account A))
			((eq? account (car possible-matches)) A)
			(else (find-or-add account
							   (cdr possible-matches)))))
	(set! (accounts L) (find-or-add acct A))))

(define-method (+! (L <ledger>) (t <transaction>))
  (+! L (from-account t))
  (+! L (to-account t))
  (set! (transactions L) (cons t (transactions L)))
  L)

(define-method (transactions (account <symbol>) (L <ledger>))
  (filter (lambda (t) (or (equal? (from-account t) account)
						  (equal? (to-account t) account)))
		  (transactions L)))

(define-method (balance (account <symbol>) (L <ledger>))
  (let ((transaction-list (transactions account L)))
	(let ((froms (filter (lambda (t) (equal? (from-account t)
											 account))
						 transaction-list))
		  (tos (filter (lambda (t) (equal? (to-account t)
										   account))
					   transaction-list)))
	  (- (apply + (map asset tos))
		 (apply + (map asset froms))))))
