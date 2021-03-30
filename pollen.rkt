#lang racket

(require pollen/decode)
(require pollen/tag)
(require txexpr)
(require sugar)

(require "template-helpers.rkt")
(provide (all-from-out "template-helpers.rkt"))

(provide (all-defined-out))


; Define some of the custom tags as block
(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append default-block-tags '(dummy svg ق سرخی دستخط شاعری-میں-سرخی))))


; Helper functions
(define (is-br? e)
  (equal? '(br) e))

(define (has-qitah-mark? tx)
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
      empty ; return an empty list if there are no note-items
      `(section ((class "footnotes")) (ol ,@note-items))))


; Custom ◊حم tag (حم = abbreviation of حاشیہ منجانب)
; To be used to denote the author of a footnote
(define (حم name)
  `(span ((class "fn-author")) ,(string-append "—" name)))

(define (سرخی . heading)
  `(h2 ,@heading))

(define (شاعری-میں-سرخی . content)
  `(h6 ,@content))

(define (ربط url . text)
  `(a ((href ,url)) ,@text))

(define (دستخط . content)
  `(p ((class "signature")) ,@content))

(define (درمیان . content)
  `(p ((class "center")) ,@content))

(define (علامت sign)
  `(span ((class "poetic-sign")) ,sign))

(define (عربی . content)
  `(span ((lang "ar") (class "ar")) ,@content))

(define (انگریزی . content)
  `(span ((lang "en") (dir "ltr") (class "en")) ,@content))

; خک = abbreviation of خط کشیدہ
(define (خک . content)
  `(i ((class "overline")) ,@content))

#|
Custom tags for handling poetry.

The generic شاعری tag does all the heavy lifting. Other convenience
functions call شاعری with a specific CSS class name for styling variations.
|#
(define (غزل . content)
  (apply شاعری #:class "ghazal" content))

(define (رباعی . content)
  (apply شاعری #:class "rubai" content))

(define (قطعہ . content)
  (apply شاعری #:class "nazm" content))

(define (قصیدہ . content)
  (apply شاعری #:class "nazm" content))

(define (مثنوی . content)
  (apply شاعری #:class "nazm" content))

(define (سلام . content)
  (apply شاعری #:class "nazm" content))

(define (سہرا . content)
  (apply شاعری #:class "nazm" content))

(define (مخمس . content)
  (apply شاعری #:class "mukhammas" content))

(define (مرثیہ . content)
  (apply شاعری #:class "musaddas" content))

(define (شعر . content)
  (apply شاعری #:class "couplet" content))
#|
A description of what is happening in the شاعری tag:

We first process `content` with decode-elements, and use `decode-paragraphs` for all its
txexprs. When calling `decode-paragraphs`, we look at the `class-name` argument to see
whether the poetry content is a rubai or a couplet, and if yes, use the #:force? option to
always have a 'p tag around `content`. This is necessary because a rubai/couplet is just
four/two lines separated by line breaks with no implicit paragraph break. We also exclude:
  (i) the 'span tag because it will have come from the processing of footnote references, and
 (ii) the 'i tag because it will have come from the processing of خک tag.

All this will give us `content-with-paras`, which will have something like this:

'((p "Line 1 of stanza 1" (br) "Line 2 of stanza 1")
  (p "Line 1 of stanza 2" (br) "Line 2 of stanza 2")
  (ق "Line 1 of stanza 3" (br) "Line 2 of stanza 3")
  (p "Line 1 of" (span [footnote reference here]) "stanza 4") (br) "Line 2 of stanza 4")
  (h6 "Some heading")
  (p "Line 1 of stanza 5" (br) "Line 2 of stanza 5")

- Each p tag represents a stanza. (Here, I use the word "stanza" to merely denote a group
  of "lines" (مصرعے). Depending on the genre, the "stanza" may be a شعر, رباعی, بند, مخمس, etc. and
  may contain more than two lines.)
- A ق tag is also a stanza, but one that says that there should be a qitah mark shown against
  this stanza.
- There may be a footnote reference in a 'span in a line of a stanza. This will have come from
  the processing of the ◊ح tag.
- An h6 tag is for things like "مطلعِ ٹانی" or "غزل" among a qaseedah/qitah/nazm/etc. This will have
  come from the processing of the ◊شاعری-میں-سرخی tag.

Next, we run `decode-elements` again on `content-with-paras`. We specify that its block tags 
('p and 'ق) should be processed with `process-single-stanza`. We exclude the h6 tag because it
doesn't need any processing. (Description of `process-single-stanza` is stated with its definition.)

This will give us `poetry-tx-elems`, which are then wrapped in a div, and given a CSS class name for
styling.
|#
(define (شاعری #:class [class-name #f] . content)
  (define (force-paras? x)
    (or
     (equal? x "rubai")
     (equal? x "couplet")))

  (define content-with-paras
    (decode-elements content
                     #:txexpr-elements-proc (lambda (x) (decode-paragraphs x #:force? (force-paras? class-name)))
                     #:exclude-tags '(span i)))

  (define poetry-tx-elems
    (decode-elements content-with-paras
                     #:block-txexpr-proc process-single-stanza
                     #:exclude-tags '(h6)))

  (define poetry-tx (txexpr 'div empty poetry-tx-elems))

  (if class-name
      (attr-set poetry-tx 'class (string-append "poetry " class-name))
      (attr-set poetry-tx 'class "poetry")))

; This function takes a single stanza (either a 'p or a 'ق)
(define (process-single-stanza stanza)
  ; Take out the elements of a `stanza`
  (define stanza-elems (get-elements stanza))
    
  ; Split its lines by using '(br) as a separator
  (define lines (filter-split stanza-elems is-br?))
    
  ; Tag the lines with a 'span tag and give them a CSS class for styling
  (define tagged-lines (map (lambda(x) (txexpr 'span '((class "line")) x)) lines))
    
  ; Insert '(br) again between the tagged lines, and combine them in a 'p tag with 
  ; a specific CSS class name for styling. If the `stanza` was a 'ق, give it an
  ; additional CSS class. 
  (if (has-qitah-mark? stanza)
      (txexpr 'p '((class "stanza has-qitah-mark")) (add-between tagged-lines '(br)))
      (txexpr 'p '((class "stanza")) (add-between tagged-lines '(br)))))


#|
The root function
|#
(define (root . elements)
  ; If footnote-block (which is a function) returns a txexpr, include it at the end of elements
  (define content (if (txexpr? (footnote-block))
                      `(,@elements ,separator-ornament ,(footnote-block))
                      elements))

  ; Now run decode-elements on content
  (txexpr 'root empty (decode-elements content
                                       #:txexpr-elements-proc decode-paragraphs
                                       #:string-proc urdu-smart-quotes)))
