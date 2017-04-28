#lang racket

(require s-lib)

(provide wrap-read)


(define (make-inter-readtable)
  (make-readtable (current-readtable)
                  ;; register our function to be called when #@ is encountered
                  #\@ 'dispatch-macro
                  ;; name of the function to be called
                  read-str))


(define (wrap-read do-read)
  (parameterize ([current-readtable (make-inter-readtable)])
    (do-read)))


(define read-str
  (case-lambda
    [(ch in src line col pos)
     ;; The caller wants a syntax object. The current position of the `in` port
     ;; is just after the #@ characters, before the opening quote.
     (let*
         ;; Here we read the next expression (which we assume is a string)
         ([literal-string (read in)])
       ;; then we parse the contents of the string and return the result,
       ;; wrapped in a new syntax object. The `in` port is now just after the
       ;; closing quote and the parser continues from there.
       (datum->syntax #f (parse literal-string)))]

    [(ch in)
     ;; The caller wants a simple list. Let's parse the input into syntax and
     ;; transform to a list (this saves as writing the same code twice).
     (syntax->datum (read-syntax #f in))]))


(define (parse str)
  ;; Assume that @{} cannot be nested and that the braces are always matched.
  ;; Obviously, in a real parsing function, these assumptions would need to be validated.
  (define lst (regexp-split #rx"@{" str))
  ;; After splitting we have a list like this: '("An error occured at " "file}:" "line}.")
  ;; We'll go over it, building a list of expressions to be passed to string-append as we go.
  (define chunks
    (for/fold ([result '()])
              ([chunk (rest lst)])        ; we don't need the first element here
      (let*
          ;; convert a string into a port
          ([is (open-input-string chunk)]
           ;; call original read to get the expression from inside brackets
           [form (read is)]
           ;; read what remains in the port back into a string
           [after-form (port->string is)]
           ;; drop the closing brace
           [after-brace (substring after-form (add1 (s-index-of after-form "}")))])
        ;; ~a is a generic "toString" function in Racket
        (append result `((~a ,form) ,after-brace)))))
  ;; chunks now looks like this: ((~a file) ":" (~a line) ".")
  `(string-append ,(first lst) ,@chunks))
