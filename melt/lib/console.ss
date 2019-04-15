#!chezscheme
(library
  (melt lib console)
  (export gem:command
          gem:text
          gem:display
          gem:format
          gemd:info
          gemd:warn
          gemd:error
          gemd:help
          gemd:item)
  (import (scheme)
          (melt utils)
          (melt cell))

  ;; apply one shell sequence, and it won't reset it
  (define (gem:command shell-sequence)
    (string-append (string #\033) shell-sequence))

  ;; generate code sequence text string
  (define (gem:text shell-sequence text)
    (string-append (gem:command shell-sequence)
                   text
                   (gem:command "[0m")))

  ;; just append strings and display it
  (define gem:display
    (lambda args
      (display (apply string-append args))))

  ;; first is the format string, others are inserted strings
  ;; (gem:format template-string assoc-list)
  ;; assoc-list: ((code-sequence . str) ... )
  (define gem:format
    (lambda args
      (apply format
             (flatten (list #t (car args) (map gem:text (map car (cadr args)) (map cdr(cadr args))))))))

  ;; display info text
  (define (gemd:info text)
    (gem:display (gem:text "[37m" "[")
                 (gem:text "[38;5;155m" "info")
                 (gem:text "[37m" "] ")
                 text "\n"))

  ;; display warn text
  (define (gemd:warn text)
    (gem:display (gem:text "[37m" "{")
                 (gem:text "[38;5;220m" "warn")
                 (gem:text "[37m" "} ")
                 text "\n"))

  ;; display error text
  (define (gemd:error text)
    (gem:display (gem:text "[37m" "(")
                 (gem:text "[38;5;197m" "error")
                 (gem:text "[37m" ") ")
                 text "\n"))

  ;; display list text
  (define (gemd:item text)
    (gem:display (gem:text "[37m" "|")
                 (gem:text "[38;5;103m" "item")
                 (gem:text "[37m" "| ")
                 text "\n"))

  ;; display help info
  (define (gemd:help text)
    (gem:display (gem:text "[37;1m" "%")
                 (gem:text "[38;5;123m" "help")
                 (gem:text "[37;1m" ">> ")
                 text "\n"))

  )
