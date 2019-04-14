(library (melt lib string)
         (export string-reverse
                 string-trim
                 string-split)
         (import (scheme))

         (define (string-reverse str)
           (if (string? str)
               (list->string (reverse (string->list str)))
               (error str "should accept a string")))

         (define (string-trim str . symbol)
           (define (%%string-trim str-ls)
             (if (not (null? str-ls))
                 (if (char-whitespace? (car str-ls))
                     (%%string-trim (cdr str-ls))
                     str-ls)
                 (error str "string list is empty")))
           (cond
             [(and (string? str) (null? symbol))
              (list->string (%%string-trim (string->list str)))]
             [(and (string? str) (eq? (car symbol) 'pre))
              (string-trim str)]
             [(and (string? str) (eq? (car symbol) 'suf))
              (string-reverse (string-trim (string-reverse str)))]
             [(and (string? str) (eq? (car symbol) 'both))
              (string-reverse (string-trim (string-reverse (string-trim str))))]))

         ;; split string into a list of sub strings, seperated by ch
         (define (string-split str ch)
           (let ((len (string-length str)))
             (letrec
               ((split
                  (lambda (a b)
                    (cond
                      ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                      ((char=? ch (string-ref str b)) (if (= a b)
                                                          (split (+ 1 a) (+ 1 b))
                                                          (cons (substring str a b) (split b b))))
                      (else (split a (+ 1 b)))))))
               (split 0 0))))

         )
