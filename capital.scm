(use-modules (ice-9 optargs)
			 (srfi srfi-19)
			 (oop goops))

(define-class <unit> ()
  (symbol #:init-keyword #:symbol))

(define-class <capital> ()
  (scalar #:init-keyword #:scalar
		  #:accessor scalar)
  (unit #:init-keyword #:unit
		#:accessor unit))

(define-class <account> ()
  (name #:init-keyword #:symbol)
  (capital #:init-keyword #:capital #:accessor capital)
  (transactions #:init-form '() #:accessor transactions))

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
  (accounts #:init-form '() #:accessor accounts)
  (transactions #:init-form '() #:accessor transactions))


(define-method (- (x <number>) (C <capital>))
  (make <capital> #:unit (unit C) #:scalar (- (scalar C))))

(define-method (+ (C1 <capital>) (C2 <capital>))
  (make <capital>
	#:unit (unit C1)
	#:scalar (+ (scalar C1) (scalar C2))))

(define-method (- (C1 <capital>) (C2 <capital>))
  (make <capital>
	#:unit (unit C1)
	#:scalar (+ (scalar C1) (- (scalar C2)))))


(define-method (add-capital (C <capital>) (A <account>))
  (set! (capital A) (+ (capital A) C)))

(define-method (remove-capital (C <capital>) (A <account>))
  (set! (capital A) (- (capital A) C)))

(define-method (process-transaction (T <transaction>) (A <account>))
  (cond
   ((eq? A (from-account T))
	(set! (transactions A) (cons T (transactions A)))
	(remove-capital (capital T) A))
   ((eq? A (to-account T))
	(set! (transactions A) (cons T (transactions A)))
	(add-capital (capital T) A))
   ((#t))))
	

(define-method (add-account (A <account>) (L <ledger>))
  (set! (accounts L) (cons A (accounts L))))

(define-method (add-transaction (T <transaction>) (L <ledger>))  
  (map
   (lambda (acct) (if (not (memq acct (accounts L)))
					  (add-account acct L)))
   (list (from-account T) (to-account T)))
  (map (lambda (acct) (process-transaction T acct))
	   (accounts L))
  (set! (transactions L) (cons T (transactions L))))


;; (define (new-transaction from to capital
;; 						 #:key (check-accounts #t))
;;   (let ((new (make <transaction>
;; 				#:from-account from
;; 				#:to-account to
;; 				#:capital capital)))
;; 	(if (not
;; 		 (throw 'unknown-account
;; 	(set! %transaction (cons new %transactions))
;; 	new))
					
					
