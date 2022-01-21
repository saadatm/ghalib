#lang racket

(require pollen/unstable/typography)

(provide (all-defined-out))

(define (urdu-smart-quotes str)
  (smart-quotes str
                #:double-open "”" #:double-close "“"
                #:single-open "’" #:single-close "‘"))

#|
We shall be deploying this to GitHub Pages, where the URL is of
the scheme <username>/github.io/<reponame>/. This messes with
hyperlinks starting with / (which is what we are using). So, we
have written a `link-prefix` function that will check whether:
  (i) the link starts with '/' (confirming that it's an internal
      link and not linking to external resources), and
 (ii) Pollen has been started with the environment variable POLLEN
      set to the value `GH_PAGES`.
If both of the above conditions are met, the function will prefix
the link with '/ghalib' (the repo name).
|#
(define (for-gh-pages?)
  (equal? (getenv "POLLEN") "GH_PAGES"))

(define (link-prefix link)
  (if (and (equal? (string-ref link 0) #\/) (for-gh-pages?))
    (string-append "/ghalib" link)
    link))
