(library (melt command init)
  (export init)
  (import (scheme)
		  (melt structure)
		  (melt lib console))

  (import type-command)

  (define (init-help)
	(gem-display (gem "[37;1m" "melt")
				 (gem "[38;5;67m" " init ")
				 (gem "[38;5;253m" "name \n"))
	(gem-display "if the"
				 (gem "[38;5;253m" " name ")
				 "is not provided, it will generate .melt in working directory \n"))

  ;; create some files
  (define (initprocedure)
	(mkdir ".melt")
	(system "touch .melt/settings.scm")
	(system "touch .melt/melt.scm")
	(mkdir ".melt/fasl")
	(mkdir ".melt/resource"))


  ;; todo 
  (define (build-welcome-message)
	'todo)
  
  ;; interface to be invoked in ui
  (define init-cli
	(lambda args
	  (cond
	   [(null? args)
		(init-help)]
	   [(string? (car args))
		(let ((name (car args)))
		  (cond
		   [(equal? name "-h")
			(init-help)]
		   [(equal? name ".")
			(initprocedure)]
		   [else
			(begin
			  (mkdir name)
			  (cd name)
			  (initprocedure))]))])))
  
  (define init
	(make-command 'init
				  "init command to generate basic settings"
				  init-cli)))
