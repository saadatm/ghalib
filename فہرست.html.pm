#lang pollen

◊(require pollen/pagetree
          pollen/template
          "helpers-template.rkt")

◊(define-meta title "فہرست")

◊(let () (current-pagetree (get-pagetree "index.ptree")) "")

◊(define (make-toc-li pagenode hlevel)
  (define node-children (children pagenode))
  ◊li{
    ◊(if node-children
      ◊(cond
        [(= hlevel 1) ◊h1{◊(node->link pagenode)}]
        [(= hlevel 2) ◊h2{◊(node->link pagenode)}]
        [(= hlevel 3) ◊h3{◊(node->link pagenode)}]
        [(= hlevel 4) ◊h4{◊(node->link pagenode)}])
      ◊(node->link pagenode))
    ◊(if node-children
      (apply ul (map (lambda (x) (make-toc-li x (+ 1 hlevel))) node-children))
      "")})

◊سرخی{فہرستِ عنوانات}

◊(apply ul #:class "toc-full" 
  (map (lambda (x) (make-toc-li x 2))
    '(سخن-ہائے-گفتنی.html
      دیوان-غالب-سے-کلیات-غالب-تک.html
      دیباچہ.html
      اتحاد-عمل.html
      اس-ویب-کتاب-کے-بارے-میں.html
      دیوان/index.html
      ضمیمہ-اول/index.html
      ضمیمہ-دوم/index.html
      ضمیمہ-سوم/index.html
      ضمیمہ-چہارم/index.html
      کتابیات.html)))
