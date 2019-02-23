(library (melt command)
  (export %builtin-commands
		  %user-commands

		  add-command
		  show-commands
		  command-query)
  (import (scheme)
		  (melt data)
		  (melt lib color)
		  (melt structure))

  (import type-command)
  (import type-data)

  (define %builtin-commands (create-data))

  (define %user-commands (create-data))

  (define (add-command command . command-data )
	(update-data!  (if (null? command-data)
					   %user-commands
					   (car command-data))
				   (create-data (list (command-name command))
								(list command))))

  ;; remove one command in command-data
  (define (remove-command command . command-data)
	(update-data! (if (null? command-data)
					   %user-commands
					   (car command-data))
				  (list (command-name command))))

  ;; display command names and its description
  (define (show-commands commands)
	(define (show-command command)
	  (cdisplay (ctext "[2C" "")
				(ctext "[37;1m" (symbol->string (command-name command)))
				":\n"
				(ctext "[5C" "")
				(ctext "[38;5;166m" (command-desc command))
				"\n"))
	(do ((command-list (data-keys commands) (cdr command-list)))
		((null? command-list) #t)
	  (show-command (cdr (assq (car command-list) (data-cont commands))))))

  ;; query command in commadn-datas, return command ann command help procedure
  (define (command-query command command-datas)
	(if (memv command (data-keys command-datas))
		(command-proc (cdr (assq command (data-cont command-datas))))
		#f))
  
  (add-command (make-command 'build "This is for building the page!" (lambda args
																	   (display "Hello World\n")))
			   %builtin-commands)
  (add-command (make-command 'serve "A little server!" (lambda args
														 (display "This is a server!\n"))))
  
;  (import (melt command build))
;  (add-command build $builtin-commands)


  )
