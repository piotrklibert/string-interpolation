#lang reader "reader.rkt" racket

(define line 42)
(define file "file-name.rkt")
(displayln #@"An error occured at @{file}:@{line}.")
;; Displays: An error occured at file-name.rkt:42.

(require racket/date)
(displayln #@"Current date&time: @{(date->string (current-date) #t)}")
;; Displays: Current date&time: Friday, April 28th, 2017 7:57:11pm
