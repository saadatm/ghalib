#lang racket

(require pollen/pagetree)
(require pollen/core)
(require txexpr)

(provide (all-defined-out))


; For listing the children of a page
(define (part-subnav children)
    (define subnav-items
        (for/list ([child (in-list children)])
            `(li (a ((href ,(format "/~a" child))) ,(select-from-metas 'title child)))))
    
    `(nav ((aria-label "اِس حصے کے صفحات"))
        (ul ((class "part-subnav")) ,@subnav-items)))


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
