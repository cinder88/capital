(define-module (capital date)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-19)
  #:export (make-date-kw)
  #:duplicates (merge-generics
				replace
				warn-override-core
				warn
				last))

(define* (make-date-kw #:key (nanosecond  0)
					         (second      0)
							 (minute      0)
							 (hour        0)
							 (day         0)
							 (month       0)
							 (year        0)
							 (zone-offset 0))
	(make-date nanosecond second minute hour
			   day month year zone-offset))
