◊(define parent-page (parent here))
◊(define prev-page (previous here))
◊(define next-page (next here))

◊(define (pages-nav)
    ◊nav[#:aria-label "اگلا اور پچھلا صفحہ"]{
        ◊ul[#:class "pages-nav"]{
            ◊when/splice[prev-page]{
                ◊li[#:class "prev"]{
                    ◊a[#:href ◊(link-prefix (format "/~a" prev-page))]{
                        ◊icon-prev
                        ◊span{◊(urdu-smart-quotes (select-from-metas 'title prev-page))}
                    }
                }
            }
            ◊when/splice[next-page]{
                ◊li[#:class "next"]{
                    ◊a[#:href ◊(link-prefix (format "/~a" next-page))]{
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
    <title>◊(select 'title metas) - کُلیاتِ غالبؔ</title>
    <meta name="viewport" content="width=device-width,initial-scale=1.0">
    <link rel="stylesheet" type="text/css" href="◊|(link-prefix "/assets/style.css")|">
    <!-- Twitter -->
    <meta name="twitter:card" content="summary">
    <meta name="twitter:title" content="◊(select 'title metas) - کُلّیاتِ غالبؔ">
    <meta name="twitter:image" content="◊|base-url|/assets/card-tw-square.png">
    <!-- Facebook -->
    <meta property="og:type" content="website">
    <meta property="og:title" content="◊(select 'title metas) - کُلّیاتِ غالبؔ">
    <meta property="og:image" content="◊|base-url|/assets/card-fb.png">
    <meta property="og:url" content="◊|base-url|/◊|here|">
</head>

<body>
    <header role="banner">
        ◊when/splice[prev-page]{
            <a href="◊|(link-prefix (format "/~a" prev-page))|" class="prev-in-header">◊(->html icon-prev)</a>
        }
        ◊when/splice[next-page]{
            <a href="◊|(link-prefix (format "/~a" next-page))|" class="next-in-header">◊(->html icon-next)</a>
        }
        <a class="toc-trigger" href="◊|(link-prefix "/فہرست.html")|">
            ◊(->html icon-toc)
        </a>
        <div class="search-trigger">
            ◊(->html icon-search)
        </div>
        <a class="main-head" href="◊|(link-prefix "/index.html")|">کُلّیاتِ غالبؔ</a>
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
