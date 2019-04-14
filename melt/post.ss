(library
  (melt post)
  (export post-meta-query
          post-attr-query
          create-post
          compose-post)
  (import (scheme)
          (melt structure)
          (melt utils)
          (melt data))

  (import type-post)

  ;; accept two assoc list
  (define create-post
    (case-lambda
      [(meta attr cont)
       (make-post meta attr cont)]))

  ;; use assoc list to compose a post
  (define (compose-post meta-alist attr-alist cont-sxml)
    (make-post (create-data meta-alist)
               (create-data attr-alist)
               cont-sxml))

  ;; query the data in post and return the value if exists
  (define (post-meta-query key post)
    (data-value-query key (post-meta post)))

  (define (post-attr-query key post)
    (data-value-query key (post-attr post)))

  )
