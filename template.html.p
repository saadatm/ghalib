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

◊(define (nav location)
    ◊ul[#:class ◊(if (string=? location "top")
                    "pages-nav top"
                    "pages-nav")]{
        ◊when/splice[prev-page]{
            ◊li[#:class "prev"]{
                ◊a[#:href ◊(format "/~a" prev-page)]{
                    ◊svg[#:width "24"
                         #:height "24"
                         #:viewBox "0 0 24 24"
                         #:fill "none"
                         #:stroke "currentColor"
                         #:stroke-width "2"
                         #:stroke-linecap "round"
                         #:stroke-linejoin "round"]{
                            ◊line[#:x1 "5" #:y1 "12" #:x2 "19" #:y2 "12"]
                            ◊polyline[#:points "12 5 19 12 12 19"]
                    }
                    ◊span{◊(urdu-smart-quotes (select-from-metas 'title prev-page))}
                }
            }
        }
        ◊when/splice[next-page]{
            ◊li[#:class "next"]{
                ◊a[#:href ◊(format "/~a" next-page)]{
                    ◊span{◊(urdu-smart-quotes (select-from-metas 'title next-page))}
                    ◊svg[#:width "24"
                         #:height "24"
                         #:viewBox "0 0 24 24"
                         #:fill "none"
                         #:stroke "currentColor"
                         #:stroke-width "2"
                         #:stroke-linecap "round"
                         #:stroke-linejoin "round"]{
                            ◊line[#:x1 "19" #:y1 "12" #:x2 "5" #:y2 "12"]
                            ◊polyline[#:points "12 19 5 12 12 5"]
                    }
                }
            }
        }
    })

<html lang="ur" dir="rtl">

<head>
    <meta charset="utf-8">
    <title>◊(select 'title metas) - کلیاتِ غالب</title>
    <meta name="viewport" content="width=device-width,initial-scale=1.0">
    <link rel="stylesheet" type="text/css" href="/assets/style.css">
</head>

<body>
    <header role="banner">
        <div class="menu-trigger">
            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="3" y1="12" x2="21" y2="12"></line>
                <line x1="3" y1="6" x2="21" y2="6"></line>
                <line x1="3" y1="18" x2="21" y2="18"></line>
            </svg>
        </div>
        <div class="search-trigger">
            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <circle cx="11" cy="11" r="8"></circle>
                <line x1="21" y1="21" x2="16.65" y2="16.65"></line>
            </svg>
        </div>
        <h1 class="main-head"><a href="/index.html">کلیاتِ غالبؔ</a></h1>
    </header>
    
    ◊(->html doc #:tag 'article #:attrs '((class "content")))
    ◊(->html (nav "bottom"))

</body>

</html>
