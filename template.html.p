◊(define parent-page (parent here))
◊(define prev-page (previous here))
◊(define next-page (next here))

◊(define (pages-nav)
    ◊nav[#:aria-label "اگلا اور پچھلا صفحہ"]{
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
        }
    })

<!doctype html>

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

    <main>
        ◊when/splice[(parent here)]{
            ◊(->html (make-breadcrumbs here))
        }

        <article class="content">
            ◊(->html doc #:splice? #t)
        </article>

        ◊when/splice[(children here)]{
            ◊(->html (part-subnav (children here)))
        }
    </main>

    ◊(->html (pages-nav))

</body>

</html>
