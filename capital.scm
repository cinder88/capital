(use-modules (ice-9 optargs)
			 (srfi srfi-1)
			 (srfi srfi-19)
			 (oop goops))

(define-class <unit> ()
  (symbol #:init-form 'USD
		  #:init-keyword #:symbol))

(define *default-unit* (make <unit>))

(define-class <capital> ()
  (scalar #:init-keyword #:scalar
		  #:accessor scalar)
  (unit #:init-form *default-unit*
	    #:init-keyword #:unit
		#:getter unit))

(define-method (equal? (c1 <capital>) (c2 <capital>))
  (and (equal? (scalar c1) (scalar c2))
	   (equal? (unit c1) (unit c2))))  

(define-method (- (x <number>) (C <capital>))
  (make <capital> #:unit (unit C) #:scalar (- (scalar C))))

(define-method (+ (C1 <capital>)) C1)

(define-method (+ (C1 <capital>) (C2 <capital>))
  (make <capital>
	#:unit (unit C1)
	#:scalar (+ (scalar C1) (scalar C2))))

(define-method (- (C1 <capital>) (C2 <capital>))
  (make <capital>
	#:unit (unit C1)
	#:scalar (+ (scalar C1) (- (scalar C2)))))

(define-method (- (C <capital>) (x <number>)) C)

(define-class <transaction> ()
  (date #:init-keyword #:date
		#:accessor date)
  (from-account #:init-keyword #:from-account
				#:accessor from-account)
  (to-account #:init-keyword #:to-account
			  #:accessor to-account)
  (capital #:init-keyword #:capital
			  #:accessor capital))

(define-class <ledger> ()
  (transactions #:init-form '() #:accessor transactions)
  (accounts #:init-form '() #:accessor accounts)
  (account-strict #:init-form #f #:accessor account-strict?))

(define-method (+ (L <ledger>) (acct <symbol>))
  (set! (accounts L) (delete-duplicates
					  (cons acct (accounts L)))))

(define-method (+ (L <ledger>) (t <transaction>))
  (let ((from (from-account t))
		(to (to-account t)))
	(cond ((account-strict? L)
		   (if (not (and (memq from L)
						 (memq to L)))
			   (throw 'unknown-acount)))
		  (#t
		   (+ L from)
		   (+ L to))))
  (set! (transactions L) (cons t (transactions L)))
  #t)

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
	  (- (apply + (map capital tos))
	  	 (apply + (map capital froms))))))
