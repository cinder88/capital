(define-module (capital arithmetic)
  #:use-module (capital base)
  #:use-module (oop goops)
  #:duplicates (merge-generics))

(define-method (+ a b) (+ a b))


;; unary operators on <capital>

(define-method (- (x <number>) (c <capital>))
  (make <capital> #:unit (unit c) #:scalar (- (scalar c))))
(define-method (- (c <capital>))
  (make <capital> #:unit (unit c) #:scalar (- (scalar c))))

(define-method (+ (c1 <capital>)) c1)


;; binary operators on <capital>
(define-method (+ (c1 <capital>) (c2 <capital>))
  (if (equal? (unit c1) (unit c2))
	  (make <capital>
		#:unit (unit c1)
		#:scalar (+ (scalar c1) (scalar c2)))
	  (make <asset> #:capital (list c1 c2))))

(define-method (- (c1 <capital>) (c2 <capital>))
  (+ c1 (- c2)))

(define-method (- (c <capital>) (x <number>)) c)


;; unary operators on <asset>

(define-method (- (x <number>) (a <asset>))
  (make <asset> #:capital (map - (capital a))))

(define-method (+ (a <asset>)) a)


;; binary operators on <asset>


(define-method (+ (a1 <asset>) (a2 <asset>))
  (apply + (cons a1 (capital a2))))

(define-method (- (a1 <asset>) (a2 <asset>))
  (+ a1 (- a2)))


;; binary operators on <asset> and <capital>


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
