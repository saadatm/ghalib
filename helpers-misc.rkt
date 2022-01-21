#lang racket

(require txexpr/base)

(provide (all-defined-out))

#|
Smart quotes (and a helper function). Adapted for Urdu from:
https://github.com/mbutterick/pollen/blob/816ce0f7af739b09dc0d64852d905ece24662bde/pollen/unstable/typography.rkt#L59

Also see the discussion at https://github.com/mbutterick/pollen/issues/266
|#
(define ((make-replacer query+replacement) str)
  (for/fold ([str str])
            ([qr (in-list query+replacement)])
    (match-define (list query replacement) qr)
    (regexp-replace* query str replacement)))

(define (urdu-smart-quotes x
                           #:apostrophe [apostrophe-str "’"]
                           #:single-open [single-open-str "’"]
                           #:single-close [single-close-str "‘"]
                           #:double-open [double-open-str "”"]
                           #:double-close [double-close-str "“"])

  (define sentence-ender-exceptions (regexp-quote ",.:;?!])}،۔؛؟"))
  (define (at-beginning-pat str)
    (pregexp (format "(?<!\\w)~a(?=\\S)" (regexp-quote str))))
  (define (at-end-pat str)
    (pregexp (format "(?<=\\S)~a(?!\\w)" (regexp-quote str))))
    
  (define quotes
    (list
     (list #px"(?<=\\w)'(?=\\w)" apostrophe-str) ; apostrophe
     (list (pregexp (format "(?<!\\w)'(?=[~a])" sentence-ender-exceptions)) single-close-str) ; sentence ender on outside exceptions
     (list (at-beginning-pat "'") single-open-str) ; single_at_beginning
     (list (at-end-pat "'") single-close-str) ; single_at_end
     (list (pregexp (format "(?<!\\w)\"(?=[~a])" sentence-ender-exceptions)) double-close-str) ; sentence ender on outside exceptions
     (list (at-beginning-pat "\"") double-open-str) ; double_at_beginning
     (list (at-end-pat "\"") double-close-str))) ; double_at_end

  (match x
    [(? string?) ((make-replacer quotes) x)]
    [(? txexpr?)
     ;; convert the quotes as if the txexpr were a flat string, to get proximity right
     ;; then replace the actual strings with substrings from this converted result
     ;; todo: handle entities & chars correctly, for now they are ignored
     (define flat-str (string-append* (filter string? (flatten (remove-attrs x)))))
     (define char-vec (for/vector #:length (string-length flat-str)
                        ([c (in-string (urdu-smart-quotes flat-str))])
                        c))
     (define offset 0)
     (let loop ([x x])
       (match x
         [(? txexpr?) (txexpr (get-tag x) (get-attrs x) (map loop (get-elements x)))]
         [(? string?)
          (define prev-offset offset)
          (set! offset (+ prev-offset (string-length x)))
          (list->string
           (for/list ([c (in-vector char-vec prev-offset offset)])
             c))]
         [_ x]))]
    [_ x]))

(define (for-gh-pages?)
  (equal? (getenv "POLLEN") "GH_PAGES"))

#|
We shall be deploying this to GitHub Pages, where the URL is of
the scheme <username>/github.io/<reponame>/. This messes with
hyperlinks starting with / (which is what we are using). So, we
have written a `link-prefix` function that will check whether:
  (i) the link starts with '/' (confirming that it's an internal
      link and not linking to external resources), and
 (ii) Pollen has been started with the environment variable POLLEN
      set to the value `GH_PAGES`.
If both of the bove conditions are met, the function will prefix
the link with '/ghalib' (the repo name).
|#
(define (link-prefix link)
  (if (and (equal? (string-ref link 0) #\/) (for-gh-pages?))
    (string-append "/ghalib" link)
    link))
