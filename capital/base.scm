(define-module (capital base)
  :use-module (oop goops)
  #:use-module (ice-9 r5rs)
  #:export (<capital> scalar unit)
  #:export (<asset> capital)
  #:export (<transaction> date from-account to-account asset)
  #:export (<ledger> transactions accounts account-strict)
  #:export (value +! asset balance)
  #:duplicates (merge-generics
				replace
				warn-override-core
				warn
				last))


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
  (name #:init-keyword #:name
		#:accessor name)
  (tags #:init-keyword #:tags
		#:accessor tags)
  (date #:init-keyword #:date
		#:accessor date)
  (from-account #:init-keyword #:from-account
				#:accessor from-account)
  (to-account #:init-keyword #:to-account
			  #:accessor to-account)
  (asset #:init-keyword #:asset
			  #:accessor asset))

(define-class <ledger> ()
  (accounts #:init-form '()
			#:init-keyword #:accounts
			#:accessor accounts)
  (transactions #:init-form '()
				#:init-keyword #:transactions
				#:accessor transactions))

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
  (filter (λ (t) (or (equal? (from-account t) account)
					 (equal? (to-account t) account)))
		  (transactions L)))

(define-method (asset (account <symbol>) (L <ledger>))
  (let ((transaction-list (transactions account L)))
	(let ((froms (filter (λ (t) (equal? (from-account t)
										account))
						 transaction-list))
		  (tos (filter (λ (t) (equal? (to-account t)
									  account))
					   transaction-list)))
	  (- (apply + (map asset tos))
		 (apply + (map asset froms))))))

(define-method (balance (account <symbol>) (l <ledger>))
  (value (asset account l)))

(define-method (balance (l <ledger>))
  (map (λ (a) (cons a (value (asset a l))))
	   (accounts l)))
