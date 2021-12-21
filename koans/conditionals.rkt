#lang racket/base

(require rackunit)


;; `if` evaluates exactly two expressions, in order: The test expression,
;; and a matching expression. The matching expression is then returned.
(define counter 0)
(check-equal?
  (if #t "yay" (set! counter 1))
  "yay")

;; Given how 'if' is evaluated, does anything happen to 'counter'?
;; Reflect on how using `if` differs from applying a procedure.
(check-equal? counter 0)

;; Logical operators condense related tests and support short circuiting.
;; If you've seen things like `(a && b)` or `(a || b)`, then that's what
;; you are dealing with here.
;;
;; - `and` returns a value corresponding to logical #t (not necessarily #t) if
;;    all following tests are #t. It stops at the first #f.

;; - `or` returns a value corresponding to logical #t (not necessarily #t) if at
;;   least one following test is #t. It stops at the first #t.

(check-equal? (and #t #t)    #t)
(check-equal? (or  #f #t)    #t)
(check-equal? (and #f #t #t) #f)
(check-equal? (or  #f #f)    #f)

;; Don't forget that conditionals return values. There may be gotchas.
(check-equal? (or (and #f #t) (and #t (or "nah" #f))) "nah")

;; You may negate results with `not`.
(check-equal? (not #f) #t)
(check-equal? (not #t) #f)
(check-equal? (not "nah") #f)

;; The `cond` form evaluates tests in sequence, and will only evaluate the
;; expressions that follow the first test that results in #t. The `else`
;; clause can be used in a `cond` to handle a default case.
;;
;; Remember that you can switch bracket styles to help readers not get lost
;; in an ocean of parentheses. Square brackets are used in the Racket Guide
;; to more strongly emphasize which expressions provided directly to `cond`.
(define key "a")
(check-equal?
  (cond
    [(equal? "a" key) 1]
    [(equal? "b" key) 2]
    [else 3])
  1)
