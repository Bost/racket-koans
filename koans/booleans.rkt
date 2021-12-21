#lang racket/base

(require rackunit)

;; Racket uses #t and #f to represent boolean values.
(check-equal? (boolean? "t") #f)
(check-equal? (boolean? "f") #f)

;; Case does not matter for boolean constants
(check-equal? (boolean? "t") #f #F)
(check-equal? (boolean? "f") #f #F)
(check-equal? (boolean? "T") #f #F)
(check-equal? (boolean? "F") #f #F)

;; Test expressions evaluate to a value based ultimately on booleans.
(check-equal? (if #t "when true" "otherwise") "when true")

;; When testing, anything other than #f counts as #t
(check-equal? (if "false" "yay" "nay") "yay")
(check-equal? (if null "yay" "nay") "yay")
(check-equal? (if '() "yay" "nay") "yay")

;; `null` and `'()` are synonyms
(check-equal? null '())
