(library
  (melt lesh)
  (export )
  (import (scheme))

  ;; help system will
  (module lesh-system
          [help-type]

          ;; help-type will tell the basic information for the type
          (define (help-type arg)
            (cond
              [(string? arg) (cond
                               ([equal? arg "site"]
                                (display "this is the help doc for site!\n"))
                               (else (type-list)))]
              [else (display "Sorry! No doc provided!\n")]))

          (define (type-list)
            (display "Types are listed :\n")
            (display "|| site || reader || page || post || asset ||\n")))
  )
