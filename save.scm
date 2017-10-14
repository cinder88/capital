(define-module (capital save)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (capital base)
  #:export (obj->list)
  #:duplicates (merge-generics))

(define-method (obj->list (p <pair>))
  `(list ,(map obj->list p)))

(define-method (obj->list (c <capital>))
  `(make <capital>
	 #:scalar ,(scalar c)
	 #:unit ,(obj->list (unit c))))

(define-method (obj->list (a <asset>))
  `(make <asset>
	 #:capital ,(obj->list (capital a))))
