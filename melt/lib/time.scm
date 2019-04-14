(library (melt lib time)
         (export date-and-time-string->date
                 month->number)
         (import (scheme)
                 (melt lib string))

         (define %%months-number-alist
           '(("Jan" . 1)
             ("Feb" . 2)
             ("Mar" . 3)
             ("Apr" . 4)
             ("May" . 5)
             ("Jun" . 6)
             ("Jul" . 7)
             ("Aug" . 8)
             ("Set" . 9)
             ("Oct" . 10)
             ("Nov" . 11)
             ("Dec" . 12)))

         ;; convert string like Dec to number
         (define (month->number str)
           (cdr (assp (lambda (obj)
                        (string=? obj str)) %%months-number-alist)))

         ;; convert (date-and-time) output string to a date
         (define (date-and-time-string->date str)
           (let* ((substr-ls (string-split str #\space))
                  (mon (list-ref substr-ls 1))
                  (day (list-ref substr-ls 2))
                  (time (list-ref substr-ls 3))
                  (year (list-ref substr-ls 4)))
             (let* ((time-str-ls (string-split time #\:))
                    (hour (list-ref time-str-ls 0))
                    (minute (list-ref time-str-ls 1))
                    (second (list-ref time-str-ls 2)))
               (make-date 0
                          (string->number second)
                          (string->number minute)
                          (string->number hour)
                          (string->number day)
                          (month->number mon)
                          (string->number year)))))

         )

