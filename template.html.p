<!doctype html>

◊;{
    Defining `here-prime` so that our pagetree will work on Windows.
    For details, see: https://github.com/mbutterick/pollen-users/issues/7#issuecomment-527888595
    As MB notes in a later comment, it is prudent to use Racket's path manipulation functions
    instead of just doing a regexp-replace, but I couldn't get them to work; so throwing caution to
    the wind and just going ahead with this.
}
◊(define here-prime (regexp-replace* #rx"[\\]" (symbol->string here) "/"))
◊(define parent-page (parent here-prime))
◊(define prev-page (previous here-prime))
◊(define next-page (next here-prime))

◊(define (nav)
    ◊ul[#:class "pages-nav"]{
        ◊when/splice[prev-page]{
            ◊li[#:class "prev"]{
                ◊a[#:href ◊(format "/~a" prev-page)]{
                    ◊icon-prev
                    ◊span{◊(urdu-smart-quotes (select-from-metas 'title prev-page))}
                }
            }
        }
        ◊when/splice[next-page]{
            ◊li[#:class "next"]{
                ◊a[#:href ◊(format "/~a" next-page)]{
                    ◊span{◊(urdu-smart-quotes (select-from-metas 'title next-page))}
                    ◊icon-next
                }
            }
        }
    })


◊(define (subnav children)
    (apply ul #:class "subnav"
        (for/list ([child (in-list children)])
            (li (a #:href (format "/~a" child) (select-from-metas 'title child))))))

<html lang="ur" dir="rtl">

<head>
    <meta charset="utf-8">
    <title>◊(select 'title metas) - کلیاتِ غالب</title>
    <meta name="viewport" content="width=device-width,initial-scale=1.0">
    <link rel="stylesheet" type="text/css" href="/assets/style.css">
</head>

<body>
    <header role="banner">
        ◊when/splice[prev-page]{
            <a href="◊|(format "/~a" prev-page)|" class="prev-in-header">◊(->html icon-prev)</a>
        }
        ◊when/splice[next-page]{
            <a href="◊|(format "/~a" next-page)|" class="next-in-header">◊(->html icon-next)</a>
        }
        <a class="toc-trigger" href="#">
            ◊(->html icon-toc)
        </a>
        <div class="search-trigger">
            ◊(->html icon-search)
        </div>
        <h1 class="main-head"><a href="/index.html">کلیاتِ غالبؔ</a></h1>
    </header>
    
    ◊(->html (subnav (or (children here) null)))
    
    ◊(->html doc #:tag 'article #:attrs '((class "content")))
    ◊(->html (nav))

</body>

</html>
