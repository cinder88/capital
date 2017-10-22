(define-module (capital date)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (srfi srfi-19)
  #:export (load-ledger-file)
  #:duplicates (merge-generics
				replace
				warn-override-core
				warn
				last))

;; (define (whitespace? c)
;;   (member c '(#\tab #\linefeed #\newline
;; 			  #\vtab #\page #\return #\space ' ')))

;; (define (entry->transaction entry)
;;   (let ((split-entry (string-split entry whitespace?)))
;; 	split-entry))

(define (load-ledger-file ledger-file)
  (call-with-input-file ledger-file
	get-string-all))

(define (split-blank-lines s)
  (let ((m (string-match "\n[\n]+" s)))
	(if (not m)
		(list s)
		(cons (string-take s (match:start m))
			  (split-blank-lines
			   (string-drop s (match:end m)))))))


(define (split-ledger-string ledger-string)
  (filter (lambda (s) (not (equal? (string-length s) 0)))
		  (string-split ledger-string #\newline)))
