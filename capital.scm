(use-modules (ice-9 optargs)
			 (srfi srfi-1)
			 (srfi srfi-19)
			 (oop goops))

(define-class <unit> ()
  (name #:init-form 'USD
		#:init-keyword #:name
		#:getter name))

(define USD (make <unit> #:name 'USD))
(define USD-V (make <unit> #:name 'USD-V))

(define-class <capital> ()
  (scalar #:init-keyword #:scalar
		  #:accessor scalar)
  (unit #:init-form USD
		#:init-keyword #:unit
		#:getter unit))

;; unary operators
(define-method (equal? (c1 <capital>) (c2 <capital>))
  (and (equal? (scalar c1) (scalar c2))
	   (equal? (unit c1) (unit c2))))

(define-method (- (x <number>) (c <capital>))
  (make <capital> #:unit (unit c) #:scalar (- (scalar c))))
(define-method (- (c <capital>))
  (make <capital> #:unit (unit c) #:scalar (- (scalar c))))

(define-method (+ (c1 <capital>)) c1)

(define-method (value (c <capital>))
  (list (scalar c) (name (unit c))))

;; binary operators
(define-method (+ (c1 <capital>) (c2 <capital>))
  (if (equal? (unit c1) (unit c2))
	  (make <capital>
		#:unit (unit c1)
		#:scalar (+ (scalar c1) (scalar c2)))
	  (make <asset> #:capital (list c1 c2))))

(define-method (- (c1 <capital>) (c2 <capital>))
  (+ c1 (- c2)))

(define-method (- (c <capital>) (x <number>)) c)


(define-class <asset> ()
  (capital #:init-form '()
		   #:init-keyword #:capital
		   #:accessor capital))

;; unary operators
(define-method (- (x <number>) (a <asset>))
  (make <asset> #:capital (map - (capital a))))

(define-method (+ (a <asset>)) a)

(define-method (value (a <asset>))
  (map value (capital a)))

;; binary operators
(define-method (+ (a <asset>) (c <capital>))
  (define (add-recur list c)
	(cond ((null? list) (list c))
		  ((equal? (unit (car list)) (unit c))
		   (cons (+ (car list) c) (cdr list)))
		  (#t
		   (cons (car list) (add-recur (cdr list) c)))))
  (make <asset> #:capital (add-recur (capital a) c)))

(define-method (+ (c <capital>) (a <asset>)) (+ a c))
(define-method (- (a <asset>) (c <capital>)) (+ a (- c)))
(define-method (- (c <capital>) (a <asset>)) (+ (- a) c))

(define-method (+ (a1 <asset>) (a2 <asset>))
  (apply + (cons a1 (capital a2))))

(define-method (- (a1 <asset>) (a2 <asset>))
  (+ a1 (- a2)))


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
