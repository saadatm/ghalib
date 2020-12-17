#lang racket

(require pollen/decode)
(require pollen/tag)
(require txexpr)
(require sugar)

(provide (all-defined-out))


; Define ◊شاعری and ◊weak as block tags
(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append default-block-tags '(شاعری dummy ق سرخی))))


; Helper functions
; To to be used in filter-split. Taken from: http://docs.racket-lang.org/pollen-tfl/_pollen_rkt_.html?q=pollen-tfl#%28elem._%28chunk._~3cdetect-list-items~3e~3a1%29%29
(define (verse-break? elem)
  (define verse-separator-pattern (regexp "\n"))
  (and (string? elem) (regexp-match verse-separator-pattern elem)))

(define (is-qitah? tx)
  (equal? 'ق (get-tag tx)))

(define (western-to-urdu-digit d)
  (cond
    [(char=? d #\0) #\۰]
    [(char=? d #\1) #\۱]
    [(char=? d #\2) #\۲]
    [(char=? d #\3) #\۳]
    [(char=? d #\4) #\۴]
    [(char=? d #\5) #\۵]
    [(char=? d #\6) #\۶]
    [(char=? d #\7) #\۷]
    [(char=? d #\8) #\۸]
    [(char=? d #\9) #\۹]))

(define (number->urdu-fn-string n)
  (define s (number->string n))
  (define lst (string->list s))
  (define urdu-lst (map western-to-urdu-digit lst))
  (define urdustring (list->string urdu-lst))
  (format "؂~a" urdustring))

(define (urdu-smart-quotes str)
  (smart-quotes str
                #:double-open "”" #:double-close "“"
                #:single-open "’" #:single-close "‘"))

; Footnotes
; Taken and adapted from: https://groups.google.com/forum/#!topic/pollenpub/laWL4SWx0Zc
(define (fn-id x) (string-append "حاشیہ-" x))
(define (fnref-id x) (string-append "ح-" x))

(define fn-names null)
(define (ح name-in)
  (define name (format "~a" name-in))
  (set! fn-names (if (member name fn-names) fn-names (cons name fn-names)))
  `(span (a ((href ,(string-append "#" (fn-id name)))
            (id ,(fnref-id name)))
           ,(number->urdu-fn-string (length (member name fn-names))))))

(define fndefs (make-hash))
(define (حاشیہ name . xs)
  (hash-set! fndefs (format "~a" name) xs))

(define (footnote-block)
  (define note-items
    (for/list ([fn-name (in-list (reverse fn-names))])
              `(li ((id ,(fn-id fn-name)))
                   ,@(append
                      (list `(span ((class "fn-count")) (a ((href ,(string-append "#" (fnref-id fn-name)))) ,(number->urdu-fn-string (length (member fn-name fn-names))))))
                      (hash-ref fndefs fn-name)))))
  
  (if (empty? note-items)
    `(dummy) ; return a dummy txexpr with no elements
    `(section ((class "footnotes")) (ol ,@note-items))))


; Custom ◊حم tag (حم = abbreviation of حاشیہ منجانب)
; To be used to denote the author of a footnote
(define (حم . name)
  (txexpr 'span '((class "fn-author")) (decode-elements name
                                          #:string-proc (lambda(x) (string-append "—" x)))))

(define (سرخی . heading)
  `(h2 ,@heading))

; This function will take the elements of a 'شاعری txexpr and splice the contents of
; its nested tags (if any). 'span tags and their contents (which will have come from a
; footnotes reference) will be entirely removed.
(define (splice-poetry xs)
  (apply append (for/list ([x (in-list xs)])
                  (if (txexpr? x)
                      (if (eq? (get-tag x) 'span)
                          '()
                          (splice-poetry (get-elements x)))
                      (list x)))))

; This function finds out the longest verse in a poem and 
; returns a class name based on the length of that longest verse
(define (get-bahr-class ptx)
  ; Get poetry content in a single string
  (define temp (splice-poetry ptx))
  (define poetry-text (apply string-append temp))
  
  ; We now have the verses of the whole poem in a single string (separated by line breaks),
  ; so we split the poem into its verses (by splitting on \n)
  (define verses (string-split poetry-text (regexp "(\n)+")))

  ; Remove any double (or more) spaces left over by nested tags
  (define verses-normalized (map (lambda(x)
                               (string-normalize-spaces x)) verses))

  ; Filter each verse so that it only contains alphabets, commas, quotes, question marks,
  ; exclamation marks... and then find the longest verse
  (define verses-filtered (map (lambda(x)
                                 (string-replace x (regexp "[^آاأبپتٹثجچحخدڈذرڑزژسشصضطظعغفقکگلمنںوؤہۂۃھءئیےۓ،\"؟! ]") "")) verses-normalized))
  (define longest-verse (first (sort verses-filtered (lambda(x y) (> (string-length x) (string-length y))))))
  (define longest-verse-length (string-length longest-verse))
  (format "~a" longest-verse-length)

  (cond
    [(<= longest-verse-length 20) (format "sm ~a" longest-verse-length)]
    [(<= 21 longest-verse-length 40) (format "md ~a" longest-verse-length)]
    [(> longest-verse-length 40) (format "lg ~a" longest-verse-length)])
)

; Custom ◊شاعری tag
; Excluding span because it will be containing the footnote reference, and
; will have come from the processing of ◊ح tag.
; Excluding ◊ق because it will be processed later in split-into-verses
(define (شاعری . content)
  (txexpr 'div `((class ,(string-append "poetry " (get-bahr-class content)))) (decode-elements content
                                          #:txexpr-elements-proc process-poetry-content
                                          #:exclude-tags '(span ق))))
  

(define (process-poetry-content elems)
  (define stanzas (decode-paragraphs elems 'p
                                     #:linebreak-proc values ; using values so that nothing is done here (values will return its arguments unchanged)
                                     #:force? #t))
  (process-stanzas stanzas))


(define (process-stanzas lst)
  ; Take each stanza in the list of stanzas and split & tag its verses
  (map split-into-verses lst))


(define (split-into-verses tagged-stanza)
  ; Get the elements (which are verses and newlines) of tagged-stanza (which is a txexpr) 
  (define verses-content (get-elements tagged-stanza))
  
  ; Using "\n" as a separator, split the verses into multiple list items. Each list item will denote a verse.
  (define verses-list (filter-split verses-content verse-break?))
  
  ; Tag each verse/list item with dd
  (define tagged-verses-list (map (lambda(x) (txexpr 'dd empty x)) verses-list))
  
  ; Finally, take the tagged verses and put them in a tagged dl (denoting a stanza)
  (if (is-qitah? tagged-stanza)
      (txexpr 'dl '((class "qitah")) tagged-verses-list)
      (txexpr 'dl empty tagged-verses-list)))


(define (root . elements)
  ; If footnote-block is not '(dummy), include it at the end of elements
  (define content (if (equal? 'dummy (get-tag (footnote-block)))
                                     ;`(root ,@elements)
                                     ;`(root ,@elements ,(footnote-block))))
                                     `(,@elements)
                                     `(,@elements ,(footnote-block))))

  ; Now run decode-elements on content-with-footnotes
  ;(txexpr 'root empty (decode-elements (get-elements content-with-footnotes)
  (txexpr 'root empty (decode-elements content
                          #:txexpr-elements-proc decode-paragraphs
                          #:string-proc (compose1 urdu-smart-quotes smart-dashes))))
