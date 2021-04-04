#lang racket

(require pollen/pagetree
         pollen/core
         txexpr
         sugar/coerce)

(require "helpers-misc.rkt")

(provide (all-defined-out))

; To be used in index.html.p and فہرست.html.p
(define (node->link node)
  (define node-string (string-append "/" (->string node)))
  (define link-name (or (select-from-metas 'toc-label node)
                        (select-from-metas 'title node)))
  `(a ((href ,(format "~a" (link-prefix node-string)))) ,link-name))


; For listing the children of a page
(define (part-subnav children)
    (define subnav-items
        (for/list ([child (in-list children)])
            `(li (a ((href ,(link-prefix (format "/~a" child)))) ,(urdu-smart-quotes (select-from-metas 'title child))))))
    
    `(nav ((aria-label "اِس حصے کے صفحات"))
        (ul ((class "part-subnav")) ,@subnav-items)))


; For breadcrumbs
(define (get-ancestors node)
    (define pnode (parent node))
    (if (false? pnode)
        (cons node empty)
        (cons node (get-ancestors pnode))))

(define (make-breadcrumbs page)
    (define breadcrumb-items
        (for/list ([item (in-list (reverse (get-ancestors page)))])
            `(li (a ((href ,(link-prefix (format "/~a" item)))) ,(urdu-smart-quotes (or (select-from-metas 'breadcrumb-label item)
                                                                                        (select-from-metas 'title item)))))))

    `(ul ((class "breadcrumbs")) ,@breadcrumb-items))


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

(define separator-ornament
  `(svg ((width "132.16")
         (height "25.822")
         (viewBox "0 0 132.16 25.822")
         (class "separator-ornament")
         (role "separator"))
           (path ((class "petals")
                  (fill "none")
                  (stroke "#000")
                  (d "m115.81 10.71c-4.2693-7.11-9.6645-9.9963-16.888-9.9963-7.2239 0-12.36 3.8708-12.36 3.8708s-2.015-2.4817-3.4078-2.4239c-1.4313 0.061-3.283 2.7734-3.283 2.7734-2.2788-2.5722-5.6362-2.7831-8.1603-1.4365 0 0 4.6016 3.2801 4.6016 10.315 0 4.62-2.9256 10.383-8.4228 10.658-5.4972 0.2753-9.7859-7.7581-13.517-13.972-4.2693-7.1101-9.6644-9.9963-16.888-9.9963-7.2239 0-12.36 3.8708-12.36 3.8708s-2.015-2.4817-3.4078-2.4239c-1.4313 0.061-3.283 2.7734-3.283 2.7734-2.2788-2.5722-5.6362-2.7831-8.1603-1.4365 0 0 4.6016 3.2801 4.6016 10.315 0 7.027-7.7869 9.825-14.728 10.716 8.0722 2.4324 18.907-0.4434 23.762-8.8539 4.3136-7.6807 8.2254-14.144 13.512-14.144 5.2834 0 7.9173 6.513 7.9173 11.042 0 6.7917-4.6016 10.315-4.6016 10.315 2.5241 1.3466 5.8814 1.1361 8.1603-1.4361 0 0 1.852 2.7135 3.2833 2.7728 1.3928 0.058 3.4074-2.4239 3.4074-2.4239s4.5398 3.7358 12.302 3.7358c6.5017 0 13.932-3.5446 17.457-9.6502 4.3136-7.6806 8.2258-14.144 13.512-14.144 5.2834 0 7.9169 6.5131 7.9169 11.042 0 6.7918-4.6016 10.315-4.6016 10.315 2.5241 1.3465 5.8815 1.136 8.1603-1.4362 0 0 1.852 2.7135 3.2833 2.773 1.3927 0.057 3.4074-2.4239 3.4074-2.4239s8.5697 4.4563 13.23 2.5173c-5.0127-0.6623-10.714-7.3936-14.445-13.607z")))
           (g ((class "flowers")
               (fill "none")
               (stroke "#000"))
                 (path ((d "m38.716 9.7716c1.905 1.5258 0.11803 3.7886 0.11803 3.7886s2.8585 0.49614 2.5176 2.8336c-0.34094 2.3375-3.222 1.9964-3.222 1.9964s0.90737 2.7602-1.1952 3.597c-2.1025 0.83681-3.34-1.7921-3.34-1.7921s-1.8078 2.2893-3.7127 0.76342c-1.905-1.5258-0.11803-3.7886-0.11803-3.7886s-2.8895-0.4013-2.5176-2.8336c0.37193-2.4324 3.222-1.9964 3.222-1.9964s-0.81901-2.8115 1.1952-3.597c2.0142-0.78557 3.34 1.7921 3.34 1.7921s1.8078-2.2893 3.7127-0.76346z")))
                 (circle ((cx "34.302") (cy "15.361") (r "1.505")))
                 (path ((d "m70.096 5.9377c2.1216 1.2066 0.71347 3.7227 0.71347 3.7227s2.901 0.039577 2.9327 2.4016c0.0316 2.3621-2.8672 2.4792-2.8672 2.4792s1.331 2.5829-0.61358 3.7405c-1.9445 1.1576-3.5807-1.2435-3.5807-1.2435s-1.4245 2.5455-3.5461 1.3389-0.71347-3.7227-0.71347-3.7227-2.9167 0.05896-2.9327-2.4016c-0.01594-2.4606 2.8672-2.4792 2.8672-2.4792s-1.2518-2.6474 0.61358-3.7405c1.8654-1.0931 3.5807 1.2435 3.5807 1.2435s1.4245-2.5455 3.5461-1.3389z")))
                 (circle ((cx "66.618") (cy "12.153") (r "1.505")))
                 (path ((d "m100.22 9.7716c1.905 1.5258 0.11803 3.7886 0.11803 3.7886s2.8585 0.49614 2.5176 2.8336c-0.34094 2.3375-3.222 1.9964-3.222 1.9964s0.90737 2.7602-1.1952 3.597c-2.1025 0.83681-3.34-1.7921-3.34-1.7921s-1.8078 2.2893-3.7127 0.76342c-1.905-1.5258-0.11803-3.7886-0.11803-3.7886s-2.8895-0.4013-2.5176-2.8336c0.37193-2.4324 3.222-1.9964 3.222-1.9964s-0.81901-2.8115 1.1952-3.597c2.0142-0.78557 3.34 1.7921 3.34 1.7921s1.8078-2.2893 3.7127-0.76346z")))
                 (circle ((cx "95.805") (cy "15.361") (r "1.505"))))))