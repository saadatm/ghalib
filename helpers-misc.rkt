#lang racket

(require pollen/unstable/typography)

(provide (all-defined-out))

(define (urdu-smart-quotes str)
  (smart-quotes str
                #:double-open "”" #:double-close "“"
                #:single-open "’" #:single-close "‘"))
