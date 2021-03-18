#lang racket

(require pollen/decode)
(require pollen/tag)
(require txexpr)
(require sugar)

(provide (all-defined-out))

; Define SVG icons (to be used in the template)
(define icon-toc `(svg ((width "24")
                         (height "24")
                         (viewBox "0 0 24 24")
                         (fill "none")
                         (stroke "currentColor")
                         (stroke-width "2")
                         (stroke-linecap "round")
                         (stroke-linejoin "round"))
                             (line ((x1 "16") (x2 "3")     (y1 "6")  (y2 "6")))
                             (line ((x1 "16") (x2 "3")     (y1 "12") (y2 "12")))
                             (line ((x1 "16") (x2 "3")     (y1 "18") (y2 "18")))
                             (line ((x1 "21") (x2 "20.99") (y1 "6")  (y2 "6")))
                             (line ((x1 "21") (x2 "20.99") (y1 "12") (y2 "12")))
                             (line ((x1 "21") (x2 "20.99") (y1 "18") (y2 "18")))))

(define icon-search `(svg ((width "24")
                           (height "24")
                           (viewBox "0 0 24 24")
                           (fill "none")
                           (stroke "currentColor")
                           (stroke-width "2")
                           (stroke-linecap "round")
                           (stroke-linejoin "round"))
                               (circle ((cx "11") (cy "11") (r "8")))
                               (line ((x1 "21") (y1 "21") (x2 "16.65") (y2 "16.65")))))

(define icon-prev `(svg ((width "24")
                         (height "24")
                         (viewBox "0 0 24 24")
                         (fill "none")
                         (stroke "currentColor")
                         (stroke-width "2")
                         (stroke-linecap "round")
                         (stroke-linejoin "round"))
                            (line ((x1 "5") (y1 "12") (x2 "19") (y2 "12")))
                            (polyline ((points "12 5 19 12 12 19")))))

(define icon-next `(svg ((width "24")
                         (height "24")
                         (viewBox "0 0 24 24")
                         (fill "none")
                         (stroke "currentColor")
                         (stroke-width "2")
                         (stroke-linecap "round")
                         (stroke-linejoin "round"))
                            (line ((x1 "19") (y1 "12") (x2 "5") (y2 "12")))
                            (polyline ((points "12 19 5 12 12 5")))))


; Define some of the custom tags as block
(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append default-block-tags '(شاعری dummy ق سرخی دستخط))))


; Helper functions
; To to be used in filter-split. Taken from: http://docs.racket-lang.org/pollen-tfl/_pollen_rkt_.html?q=pollen-tfl#%28elem._%28chunk._~3cdetect-list-items~3e~3a1%29%29
(define (line-break? elem)
  (define line-separator-pattern (regexp "\n"))
  (and (string? elem) (regexp-match line-separator-pattern elem)))

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

#|
 ح tag for referring to a footnote. This tag has the following variants:
 
 1. ‏◊ح{نام-حاشیہ}
    This is the simple variant where we only mention the footnote name.
 2. ‏◊ح['شروع]{نام-حاشیہ}
    This variant takes a symbol (‏'شروع), which is then used to append an extra
    CSS class "start" (which, in turn, is used to position the footnote reference before
    its respective line/misra or couplet/stanza in the web layout).
 3. ‏◊ح['آخر]{نام-حاشیہ}
    This variant takes a symbol (‏'آخر), which is then used to append an extra
    CSS class "end" (which, in turn, is used to position the footnote reference after
    its respective line/misra or couplet/stanza in the web layout).
 4. ‏◊ح['ساکت]{نام-حاشیہ}
    This variant takes a symbol (‏'ساکت), which is then used to append an extra
    CSS class (which, in turn, is used to position the footnote reference without
    absolute positioning in the web layout). The purpose of this variant is to refer
    to those footnotes that are about a whole ghazal. In the content, this tag variant
    conventionally appears inside a درمیان tag that comes before a شاعری tag.
 |#
(define (ح . elems)
  ; Check if we have a symbol in ح (adapted from https://github.com/mbutterick/pollen-users/issues/65#issuecomment-653621118 )
  (define-values (pos name-in)
    (cond [(symbol? (car elems)) (values (car elems) (cdr elems))]
          [else (values 'none elems)]))
  
  (define name (apply string-append name-in))
  (set! fn-names (if (member name fn-names) fn-names (cons name fn-names)))
  `(span ((class ,(cond [(equal? pos 'شروع) "fn-ref start"]
                        [(equal? pos 'آخر) "fn-ref end"]
                        [(equal? pos 'ساکت) "fn-ref static"]
                        [else "fn-ref"])))
            (a ((href ,(string-append "#" (fn-id name)))
                (id ,(fnref-id name)))
               ,(number->urdu-fn-string (length (member name fn-names))))))

; The حاشیہ tag for containing the actual footnote
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

(define (دستخط . content)
  `(p ((class "signature")) ,@content))

(define (درمیان . content)
  `(p ((class "center")) ,@content))

(define (علامت sign)
  `(span ((class "poetic-sign")) ,sign))

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

; This function finds out the longest line in a poem and 
; returns a class name based on the length of that longest line
(define (get-bahr-class ptx)
  ; Get poetry content in a single string
  (define temp (splice-poetry ptx))
  (define poetry-text (apply string-append temp))
  
  ; We now have the lines of the whole poem in a single string (separated by line breaks),
  ; so we split the poem into its lines (by splitting on \n)
  (define lines (string-split poetry-text (regexp "(\n)+")))

  ; Remove any double (or more) spaces left over by nested tags
  (define lines-normalized (map (lambda(x)
                               (string-normalize-spaces x)) lines))

  ; Filter each line so that it only contains alphabets, commas, quotes, question marks,
  ; exclamation marks... and then find the longest line
  (define lines-filtered (map (lambda(x)
                                 (string-replace x (regexp "[^آاأبپتٹثجچحخدڈذرڑزژسشصضطظعغفقکگلمنںوؤہۂۃھءئیےۓ،\"؟! ]") "")) lines-normalized))
  (define longest-line (first (sort lines-filtered (lambda(x y) (> (string-length x) (string-length y))))))
  (define longest-line-length (string-length longest-line))
  (format "~a" longest-line-length)

  (cond
    [(<= longest-line-length 20) (format "sm ~a" longest-line-length)]
    [(<= 21 longest-line-length 40) (format "md ~a" longest-line-length)]
    [(> longest-line-length 40) (format "lg ~a" longest-line-length)])
)

; Custom ◊شاعری tag
; Excluding span because it will be containing the footnote reference, and
; will have come from the processing of ◊ح tag.
; Excluding ◊ق because it will be processed later in split-into-lines
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
  ; Take each stanza in the list of stanzas and split & tag its lines
  (map split-into-lines lst))


(define (split-into-lines tagged-stanza)
  ; Get the elements (which are lines and newlines) of tagged-stanza (which is a txexpr) 
  (define lines-content (get-elements tagged-stanza))
  
  ; Using "\n" as a separator, split the lines into multiple list items. Each list item will denote a line.
  (define lines-list (filter-split lines-content line-break?))
  
  ; Tag each line/list item with dd
  (define tagged-lines-list (map (lambda(x) (txexpr 'dd empty x)) lines-list))
  
  ; Finally, take the tagged lines and put them in a tagged dl (denoting a stanza)
  (if (is-qitah? tagged-stanza)
      (txexpr 'dl '((class "qitah")) tagged-lines-list)
      (txexpr 'dl empty tagged-lines-list)))


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
