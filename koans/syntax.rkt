#lang racket/base

(require
  racket/function
  rackunit
  (for-syntax racket/base syntax/parse)
  syntax/macro-testing)


;; `syntax` creates a syntax object
(check-equal? (syntax? (syntax A)) #t)

;; `#'` is short for `syntax`, similar to how `'` is short for `quote`
(check-equal? (syntax? #'A) #t)

;; `syntax-e` extracts the datum from a syntax object
(check-equal? (syntax-e #'A) 'A)
(check-equal? (syntax-e (syntax A)) 'A)

(define (first-stx stx)
  (define x* (syntax-e stx))
  (car x*))

(check-equal? (syntax-e (first-stx #'(1 2 3))) 1)

;; `#`` is short for `quasisyntax` and `#,` is short for `unsyntax`
(define four 4)

(check-equal?
 (syntax-e #`#,four)  ; value associated with `four`
 (syntax-e (quasisyntax (unsyntax four)))
 four)

(check-equal?
 (syntax-e #`#,four)  ; value associated with `four`
 4
 #;(syntax-e #'#,four))

;; Syntax objects are Racket's internal representation of Racket source code.
;;
;; When you run Racket on a file, for example `racket koans/syntax.rkt`,
;; Racket _reads_ the program into one big syntax object.
;;
;; After reading, Racket _expands_ this syntax object into a well-formed
;; program. (well-formed = no unbound identifiers, no single-armed `if`
;; statements, etc.)
;;
;; And after expanding, Racket runs the program. (The expander is a
;; program that runs to produce the program you originally wanted to
;; run.)
;;
;; `define-syntax` binds an identifier to an expansion-time function.

(define-syntax (f stx)
  #'42)
(check-equal? (f) 42
              #;"the left side _expands_ to 42")
(check-equal? f 42
              #;"the left side _expands_ to 42")

;; An expansion-time function that takes a syntax object as input and computes
;; a new syntax object is called a _syntax transformer_ or a _macro_. If `f`
;; is a macro and `(f x)` appears in a program, then the expander will invoke
;; `f` with a syntax object representing the application `(f x)`.
(define-syntax (count-args stx)
  (define fn-and-args-list (syntax-e stx))
  (define args-list (cdr fn-and-args-list))
  (define num-args (length args-list))
  #`#,num-args)

(check-equal? (count-args) 0
              "`count-args` applied to #'(count-args)")
(check-equal? (count-args 1 2 3) 3
              "`count-args` applied to #'(count-args 1 2 3)")
(check-equal? (count-args unbound-id) 1
              "`unbound-id` disappears before the program runs")

;; `syntax-parse` is a pattern-matcher for syntax objects
(define-syntax (count-args2 stx)
  (syntax-parse stx
    [(?fn ?arg* ...)
     (define args-list (syntax-e #'(?fn ?arg* ...)))
     (define num-args (length args-list))
     #`#,num-args]))

(check-not-exn
 (lambda ()
   (convert-compile-time-error ;; Does apparently nothing???
    (let ()
      (check-equal?
       (count-args2 'A) 2
       "there is an error in the definition of `count-args2`")
    (check-equal? (count-args2 a b c d) 5)))))

;; `syntax-parse` is helpful for writing macros, and macros are helpful for
;; programming your programs.
(define-syntax (check-all-equal? stx)
  "Check pairs for equality"
  (syntax-parse stx
    [(_)
     #'(void)]
    [(_ ?e0 ?e1)
     #'(check-equal? ?e0 ?e1)]
    [(_ [?e0* ?e1*] ...)
     #'(begin
         (check-equal? ?e0* ?e1*) ...)]))

(check-all-equal?)
(check-all-equal? 2 2)
(check-all-equal?
  #;[2 "there is an error in the definition of `check-all-equal?`"]
  [3 3]
  ['A 'A]
  ["hello" "hello"])
