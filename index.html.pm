#lang pollen

◊(require pollen/pagetree
          pollen/template
          "helpers-template.rkt")

◊(define-meta title "سرورق")
◊(define-meta template "template-index.html")

◊(let () (current-pagetree (get-pagetree "index.ptree")) "")

◊(define (make-toc-li pagenode [show-children? #f])
  (define node-children (children pagenode))
  ◊li{
    ◊h3{◊(node->link pagenode)}
    ◊(if (and node-children show-children?)
      (apply ul (map (compose1 li node->link) node-children))
      "")})

◊(apply ul #:class "toc-index" 
  (map
    (lambda (x) (apply make-toc-li x))
    '((سخن-ہائے-گفتنی.html)
      (دیوان-غالب-سے-کلیات-غالب-تک.html)
      (دیباچہ.html)
      (اتحاد-عمل.html)
      (اس-ویب-کتاب-کے-بارے-میں.html)
      (دیوان/index.html #t)
      (ضمیمہ-اول/index.html)
      (ضمیمہ-دوم/index.html)
      (ضمیمہ-سوم/index.html #t)
      (ضمیمہ-چہارم/index.html)
      (کتابیات.html))))
