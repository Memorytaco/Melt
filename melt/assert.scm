#!chezscheme
(library (melt assert)
  (export mmessage
		  massert)
  (import (scheme))

  ;; there are three level errors
  ;; 'error 'warn 'fatal

  ;; 'error: the input is not correct, but the program will continue
  ;; 'warn: the input may induce some error
  ;; 'fatal: the program will breakdown and quit

  ;; error format:
  ;;        <error-type>: <error-message> --> <arg-position> :<error-input> || <expect-input>
  ;; warn format:
  ;;        <warn-type>: <warn-message>
  ;; fatal format:
  ;;        <fatal-type>: <fatal-message> <fatal-stack>


  (define-syntax massert
	(syntax-rules ()
	  ))
  ;; TODO add fatal control
  (define (mmessage type msg . args)
	(define foreground (string-append (string #\033) "[38;5;"))
	(define background (string-append (string #\033) "[48;5;"))
	(define restore (string-append (string #\033) "[0m"))
	(define argposition (if (not (null? args)) (car args) -1))
	(define er-input (if (not (or (null? args) (null? (cdr args)))) (car (cdr args)) 'er-input))
	(define ex-input (if (not (or (null? args) (null? (cdr (cdr args))))) (car (cdr (cdr args))) 'ex-input))
	(cond
	 [(eq? type 'error)
	  (display (string-append foreground "196mError: " restore msg " "))
	  (display (string-append foreground "15;1m--> " restore))
	  (display argposition)
	  (display " ")
	  (display (string-append  foreground "15;1m: " restore "get "))
	  (display er-input)
	  (display " ")
	  (display (string-append  foreground "15;1m|| " restore "but expect "))
	  (display ex-input)
	  (display "\n")]
	 [(eq? type 'warn)
	  (display (string-append foreground "11m" "Warn: " restore msg "\n"))]
	 [(eq? type 'fatal)
	  (display (string-append foreground "196;1m" "FATAL: " restore msg "\n"))]
	 [else (error 'type "in (melt assert), mmessage's type should be error, warn or fatal")]))

  ;; TODO: add more inspection
  (define (massert type msg predict procedure args)
	(if (not (apply predict (list (apply procedure args))))
		(cond
		 [(eq? type 'error)
		  (mmessage 'error msg 1 'x 'y)]
		 [(eq? type 'warn)
		  (mmessage 'warn msg)]
		 [(eq? type 'fatal)
		  (mmessage 'fatal msg)])))




  )
