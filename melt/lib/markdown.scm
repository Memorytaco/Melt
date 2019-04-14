#!chezscheme
(library
  (melt lib markdown)
  (export markdown->sxml
          scone
          pattern-match
          char-forward
          position-back
          eof)
  (import (scheme))

  ;; define append but contain one single element
  (define (scone ls new-ele)
    (cond
      [(null? new-ele)
       ls]
      [else
        (append ls (list new-ele))]))

  ;; define the eof object for pattern
  (define eof (eof-object))

  ;; if n larger than position, do nothing
  (define (position-back port n)
    (let ((position (port-position port)))
      (set-port-position! port (if (and (> position 0) (> position n) (> n 0))
                                   (- position n)
                                   position))))

  ;; forward n chars
  (define (char-forward port n)
    (do ((count n (- count 1)))
      ((= count 0))
      (read-char port)))

  ;; #######################################################
  ;; the pattern is a char list. port is a text port.

  ;; pattern can include a nested char list.
  ;; normal pattern is (#\space #\a ...)
  ;; if the pattern is (#\space #a), it match " a", return the pattern means true.

  ;; and a nested pattern is (#\space (#\a #\b #\c) #\\)
  ;; this pattern match " a\" and " b\" and " c\".
  ;; the nested char match a single char.
  ;; if the nested list is '(), it will match any char, any!
  (define (pattern-match pattern port)
    (define ($$check pattern position port)
      (if (null? pattern)
          (begin (set-port-position! port position) #t)
          (let ((pattern-atom (car pattern)))
            (cond
              [(null? pattern-atom)
               (read-char port) ;; accept it, if the atom is '()
               ($$check (cdr pattern) position port)]
              [(and (list? pattern-atom)
                    (eq? (car pattern-atom) '!))
               (if (not (member (read-char port) pattern-atom))
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [(list? pattern-atom)
               (if (member (read-char port) pattern-atom)
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [(char? pattern-atom)
               (if (eq? (read-char port) pattern-atom)
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [(eof-object? pattern-atom)
               (if (eof-object? (read-char port))
                   ($$check (cdr pattern) position port)
                   (begin (set-port-position! port position) #f))]
              [else
                (error 'pattern-atoms "unproperly pattern atom")]))))
    ($$check pattern (port-position port) port))

  (define (g-check-list ls)
    (lambda (ob)
      (equal? ls ob)))

  ;; use the pattern to determine which context to enter in
  ;; return the parsed sxml and pass previous context(procedure) to it
  ;; so it can return to previous context
  ;; 这里之后可以使用cc来进行错误处理和异常报告排错, 需要和define-FA 联合

  (define (context-transform sxml port pattern-ls FAs cur-context)
    (call/cc
      (lambda (cc)
        (do ((patterns pattern-ls (cdr patterns))
             (key-context-list (map cons pattern-ls FAs)))
          ((null? patterns) #f)
          (let ((cur-pattern (car patterns)))
            (if (pattern-match cur-pattern port)
                (cc ((lambda (item) (cur-context (scone sxml item) port cur-context))
                     ((cdr (assp (g-check-list cur-pattern) key-context-list)) (list) port cur-context)))))))))

  ;; check each terminate pattern and end the context if matched returning value.
  ;; forward-numbers list is to forward the port position if matched.
  ;; if not match, return #f.
  ;; the value must use delay to wrap
  (define (context-return value port pattern-ls forward-numbers)
    (call/cc
      (lambda (cc)
        (do ((end-pattern (cons `(,eof) pattern-ls) (cdr end-pattern))
             (query-forward-numbers (map cons (cons `(,eof) pattern-ls)
                                         (cons 0 forward-numbers))))
          ((null? end-pattern) #f)
          (let* ((cur-pattern (car end-pattern))
                 (number (cdr (assp (g-check-list cur-pattern) query-forward-numbers))))
            (if (pattern-match cur-pattern port)
                (begin (char-forward port number) (cc (force value)))))))))

  ;; define a FA
  ;; end-lambda must be (lambda (sxml port) bodys...)
  ;; TODO 之后可以在else后边加入错误处理
  (define-syntax define-FA
    (syntax-rules ()
      [(_ name pre-lambda end-lambda (end-patterns number-list) (trans-patterns FAs) )
       (define name
         (lambda (sxml port pre-context)
           (if (equal? pre-context name)
               (cond
                 [(context-return (delay (end-lambda sxml port))
                                  port end-patterns number-list)]
                 [(context-transform sxml port trans-patterns FAs name)]
                 [else (error 'define-FA "no proper transform context definition")])
               (name (scone sxml (pre-lambda sxml port)) port name))))]))

  ;; (g 'x 3) ==> (x x x)
  (define (g ele num)
    (cond
      [(= num 0)
       '()]
      [(> num 0)
       (cons ele (g ele (- num 1)))]))

  ;;; above is the common utility
  ;;; #################################################################
  ;;; #################################################################
  ;;; #################################################################
  ;;; below is the definition

  ;; define makrdown-> sxml
  (define (markdown->sxml port)
    (pattern-top-parse (list) port #f))

  ;;; =========================== A ===== U ====== X ======================
  ;; return the escaped char

  (define (aux-escape sxml port . idle)
    (char-forward port 1)
    (read-char port))

  ;; generate procedure which ignore 'number' chars
  (define (aux-ignore number)
    (lambda (sxml port . idle)
      (char-forward port number) '()))

  ;; read one char
  (define (aux-common sxml port . idle)
    (let ((char (read-char port)))
      (if (eof-object? char)
          '()
          char)))

  ;; the top environment(context)
  (define-FA pattern-top-parse
             (lambda (sxml port) '())
             (lambda (sxml port) sxml)
             [(list `(,eof)) '(0)]
             [(list '(#\\ ())
                    '((#\newline #\space))
                    '(#\` #\` #\`)
                    '(#\#)
                    '(#\* #\* #\*)
                    '(#\- #\- #\-)
                    '(#\- #\space #\[ (#\space) #\] #\space)
                    '(#\* #\space #\[ (#\space) #\] #\space)
                    '(#\* #\space #\[ (! #\space) #\] #\space)
                    '(#\- #\space #\[ (! #\space) #\] #\space)
                    '(#\* #\space)
                    '(#\- #\space)
                    '((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) #\. #\space)
                    '(#\! #\[)
                    '(#\> #\space)
                    '(()))
              (list aux-escape
                    (aux-ignore 1)
                    pattern-parse-block-code
                    pattern-parse-header
                    pattern-parse-hr
                    pattern-parse-hr
                    aux-parse-task-list-unchecked
                    aux-parse-task-list-unchecked
                    aux-parse-task-list-checked
                    aux-parse-task-list-checked
                    pattern-parse-ul-list
                    pattern-parse-ul-list
                    pattern-parse-ol-list
                    pattern-parse-img
                    pattern-parse-block-quote
                    pattern-parse-paragraph)])

  ;;; ==================== paragraph ===================
  ;; return the paragraph
  (define-FA pattern-parse-paragraph
             (lambda (sxml port) '())
             (lambda (sxml port) `(p ,sxml))
             [(list '(#\newline #\newline)
                    '(#\newline #\` #\` #\`)
                    '(#\newline #\#)
                    '(#\newline #\* #\space)
                    '(#\newline #\- #\space)
                    '(#\newline (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) #\. #\space)
                    '(#\newline #\! #\[)
                    '(#\newline #\> #\space))
              (list 2 1 1 1 1 1 1 1)]
             [(list '(#\\ ())
                    '(#\newline)
                    '(#\[)
                    '(#\`)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\$ #\$)
                    '(#\~ #\~)
                    '(()))
              (list aux-escape
                    aux-paragraph-space
                    pattern-parse-link
                    pattern-parse-inline-code
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-math
                    pattern-parse-strike-through
                    aux-common)])

  (define (aux-paragraph-space sxml port . idle)
    (context-return (delay #\space) port (list '(#\newline)) (list 1)))

  ;; =====================  block-quote =======================
  ;; return block quote
  (define-FA pattern-parse-block-quote
             (aux-ignore 2)
             (lambda (sxml port) `(blockquote (p ,sxml)))
             ((list '(#\newline #\newline)
                    '(#\newline #\` #\` #\`)
                    '(#\newline #\#)
                    '(#\newline #\* #\space)
                    '(#\newline #\- #\space)
                    '(#\newline (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) #\. #\space)
                    '(#\newline #\! #\[)
                    '(#\newline #\> #\space))
              (list 2 1 1 1 1 1 1 1))
             ((list '(#\\)
                    '(#\newline)
                    '(#\`)
                    '(#\[)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\$ #\$)
                    '(#\~ #\~)
                    '(()))
              (list aux-escape
                    aux-paragraph-space
                    pattern-parse-inline-code
                    pattern-parse-link
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-math
                    pattern-parse-strike-through
                    aux-common)))

  ;; ========================   list =============================
  ;; for the list context
  ;; return ul or ol
  (define-FA pattern-parse-ul-list
             (lambda (sxml port) '())
             (lambda (sxml port) (cons 'ul sxml))
             [(list '(#\newline (! #\* #\-) (! #\space))
                    '(#\newline (! #\* #\-) ()))
              (list 1 1)]
             [(list '(#\newline)
                    '(#\* #\space)
                    '(#\- #\space))
              (list (aux-ignore 1)
                    aux-parse-ul-list
                    aux-parse-ul-list)])

  (define-FA aux-parse-ul-list
             (lambda (sxml port) '())
             (lambda (sxml port) `(li ,sxml))
             [(list '(#\newline))
              (list 0)]
             [(list '(#\* #\space)
                    '(#\- #\space)
                    '(#\~ #\~)
                    '(#\$ #\$)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\`)
                    '(()))
              (list (aux-ignore 2)
                    (aux-ignore 2)
                    pattern-parse-strike-through
                    pattern-parse-math
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-inline-code
                    aux-common)])

  (define-FA pattern-parse-ol-list
             (lambda (sxml port) '())
             (lambda (sxml port) (cons 'ol sxml))
             [(list '(#\newline (! #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) (! #\.) (! #\space))
                    '(#\newline (! #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) ()))
              (list 1 1)]
             [(list '(#\newline)
                    '((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) #\. #\space))
              (list (aux-ignore 1)
                    aux-parse-ol-list)])

  (define-FA aux-parse-ol-list
             (lambda (sxml port) '())
             (lambda (sxml port) `(li ,sxml))
             [(list '(#\newline))
              (list 0)]
             [(list '((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0) #\. #\space)
                    '(#\~ #\~)
                    '(#\$ #\$)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\`)
                    '(()))
              (list (aux-ignore 3)
                    pattern-parse-strike-through
                    pattern-parse-math
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-inline-code
                    aux-common)])

  ;; ====================== task list =================
  (define-FA pattern-parse-task-list
             (lambda (sxml port) '())
             (lambda (sxml port) (cons 'ul sxml))
             [(list '(#\newline (! #\- #\*)))
              (list 1)]
             [(list '(#\newline)
                    '(#\- #\space #\[ (#\space) #\] #\space)
                    '(#\* #\space #\[ (#\space) #\] #\space)
                    '(#\* #\space #\[ (! #\space) #\] #\space)
                    '(#\- #\space #\[ (! #\space) #\] #\space))
              (list (aux-ignore 1)
                    aux-parse-task-list-unchecked
                    aux-parse-task-list-unchecked
                    aux-parse-task-list-checked
                    aux-parse-task-list-checked)])

  (define-FA aux-parse-task-list-checked
             (lambda (sxml port) '())
             (lambda (sxml port) `(li (input (@ (checked "")
                                                (disabled "")
                                                (type "checkbox")))
                                      ,sxml))
             [(list '(#\newline))
              (list 0)]
             [(list '(#\\)
                    '(#\- #\space #\[ (! #\space) #\] #\space)
                    '(#\* #\space #\[ (! #\space) #\] #\space)
                    '(#\~ #\~)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\`)
                    '(()))
              (list aux-escape
                    (aux-ignore 6)
                    (aux-ignore 6)
                    pattern-parse-strike-through
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-inline-code
                    aux-common)])

  (define-FA aux-parse-task-list-unchecked
             (lambda (sxml port) '())
             (lambda (sxml port) `(li (input (@ (disabled "")
                                                (type "checkbox")))
                                      ,sxml))
             [(list '(#\newline))
              (list 0)]
             [(list '(#\\)
                    '(#\- #\space #\[ (#\space) #\] #\space)
                    '(#\* #\space #\[ (#\space) #\] #\space)
                    '(#\~ #\~)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\`)
                    '(()))
              (list aux-escape
                    (aux-ignore 6)
                    (aux-ignore 6)
                    pattern-parse-strike-through
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-inline-code
                    aux-common)])


  ;;;   =============== header ===============
  ;; return the header
  (define-FA pattern-parse-header
             (lambda (sxml port) '())
             (lambda (sxml port) sxml)
             [(list '(#\newline)) (list 1)]
             [(list '(#\#))
              (list aux-parse-header)])

  (define (aux-parse-header sxml port . idle)
    (cond
      [(pattern-match '(#\newline) port)
       sxml]
      [(pattern-match '(#\# #\space) port)
       (char-forward port 2)
       (list 'h1 (aux-parse-header sxml port))]
      [(pattern-match '(#\# #\# #\space) port)
       (char-forward port 3)
       (list 'h2 (aux-parse-header sxml port))]
      [(pattern-match '(#\# #\# #\# #\space) port)
       (char-forward port 4)
       (list 'h3 (aux-parse-header sxml port))]
      [(pattern-match '(#\# #\# #\# #\# #\space) port)
       (char-forward port 5)
       (list 'h4 (aux-parse-header sxml port))]
      [(pattern-match '(#\# #\# #\# #\# #\# #\space) port)
       (char-forward port 6)
       (list 'h5 (aux-parse-header sxml port))]
      [(pattern-match '(#\# #\# #\# #\# #\# #\# #\space) port)
       (char-forward port 7)
       (list 'h6 (aux-parse-header sxml port))]
      [else (aux-parse-header (scone sxml (read-char port)) port)]))

  ;; =================  block code ==========================
  ;; return the block code
  (define-FA pattern-parse-block-code
             (lambda (sxml port) '())
             (lambda (sxml port) `(pre (code ,(car sxml) ,(car (cdr sxml)))))
             ((list '(#\` #\` #\` #\newline)) (list 4))
             ((list '(#\` #\` #\`)
                    '(()))
              (list aux-parse-block-code-type
                    aux-parse-block-code-cont)))

  (define aux-parse-block-code-type
    (lambda (sxml port . idle)
      (cond
        [(context-return (lambda () `(@ (class ,(apply string sxml))))
                         port '((#\newline)) '(1))]
        [(pattern-match '(#\` #\` #\` #\space) port)
         (char-forward port 4)
         (aux-parse-block-code-type (scone sxml (read-char port)) port)]
        [(pattern-match '(#\` #\` #\`) port)
         (char-forward port 3)
         (aux-parse-block-code-type (scone sxml (read-char port)) port)]
        [else (aux-parse-block-code-type (scone sxml (read-char port)) port)])))

  (define aux-parse-block-code-cont
    (lambda (sxml port . idle)
      (cond
        [(pattern-match '(#\` #\` #\` #\newline) port)
         sxml]
        [else (aux-parse-block-code-cont
                (scone sxml (read-char port)) port)])))

  ;; =================== inline code ===========================
  ;; return the code type
  (define-FA pattern-parse-inline-code
             (aux-ignore 1)
             (lambda (sxml port) (cons 'code sxml))
             [(list '(#\`)) (list 1)]
             [(list '(#\\)
                    '(()))
              (list aux-escape
                    aux-common)])

  ;; ==================== strong =============================
  (define-FA pattern-parse-strong
             (aux-ignore 2)
             (lambda (sxml port) (cons 'strong sxml))
             [(list '(#\* #\*)
                    '(#\_ #\_))
              (list 2 2)]
             [(list '(#\\)
                    '(()))
              (list aux-escape
                    aux-common)])

  ;; ===================== em ================================
  ;; return the emphsis
  (define-FA pattern-parse-em
             (aux-ignore 1)
             (lambda (sxml port) (cons 'em sxml))
             ((list '(#\*)
                    '(#\_))
              (list 1 1))
             ((list '(#\\)
                    '(()))
              (list aux-escape
                    aux-common)))

  ;; ====================== strike through ================================
  (define-FA pattern-parse-strike-through
             (aux-ignore 2)
             (lambda (sxml port) (list 'del sxml))
             [(list '(#\~ #\~)) (list 2)]
             [(list '(#\\)
                    '(#\* #\*)
                    '(#\_ #\_)
                    '(#\*)
                    '(#\_)
                    '(#\`)
                    '(()))
              (list aux-escape
                    pattern-parse-strong
                    pattern-parse-strong
                    pattern-parse-em
                    pattern-parse-em
                    pattern-parse-inline-code
                    aux-common)])

  ;; ====================== hr ==================================
  ;; return (hr)
  (define-FA pattern-parse-hr
             (lambda (sxml port) '())
             (lambda (sxml port) '(hr))
             ((list '(#\newline)) '(1))
             ((list '(()))
              (list aux-common)))

  ;; ====================== link ==================================
  (define-FA pattern-parse-link
             (lambda (sxml port) '())
             (lambda (sxml port) `(a ,(car (cdr sxml)) ,(car sxml)))
             ((list '(#\))) '(1))
             ((list '(#\()
                    '(#\[))
              (list aux-parse-link-end
                    aux-parse-link-begin)))

  (define (aux-parse-link-begin sxml port . idle)
    (cond
      [(pattern-match '(#\[) port)
       (char-forward port 1)
       (aux-parse-link-begin sxml port)]
      [(pattern-match '(#\]) port)
       (char-forward port 1)
       sxml]
      [else (aux-parse-link-begin (scone sxml (read-char port)) port)]))

  (define (aux-parse-link-end sxml port . idle)
    (cond
      [(pattern-match '(#\() port)
       (char-forward port 1)
       (aux-parse-link-end sxml port)]
      [(pattern-match '(#\)) port)
       `(@ (href ,(apply string sxml)))]
      [else (aux-parse-link-end (scone sxml (read-char port)) port)]))

  ;; ======================= img ================================
  ;; return an img
  (define-FA pattern-parse-img
             (lambda (sxml port) '())
             (lambda (sxml port) `(img ,(car (cdr sxml)) ,(car sxml)))
             ((list '(#\))) '(1))
             ((list '(#\()
                    '(#\! #\[))
              (list aux-parse-img-end
                    aux-parse-img-begin)))

  (define (aux-parse-img-begin sxml port . idle)
    (cond
      [(pattern-match '(#\! #\[) port)
       (char-forward port 2)
       (aux-parse-img-begin sxml port)]
      [(pattern-match '(#\]) port)
       (char-forward port 1)
       sxml]
      [else (aux-parse-img-begin (scone sxml (read-char port)) port)]))

  (define (aux-parse-img-end sxml port . idle)
    (cond
      [(pattern-match '(#\() port)
       (char-forward port 1)
       (aux-parse-img-end sxml port)]
      [(pattern-match '(#\)) port)
       `(@ (src ,(apply string sxml)))]
      [else (aux-parse-img-end (scone sxml (read-char port)) port)]))

  ;; ========== math block (need external support =======
  (define-FA pattern-parse-math
             (aux-ignore 2)
             (lambda (sxml port) (list 'span '(@ (class "math")) sxml))
             [(list '(#\$ #\$)) (list 2)]
             [(list '(()))
              (list aux-common)])

  )
