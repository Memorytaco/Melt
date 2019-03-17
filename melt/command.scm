(library (melt command)
  (export %builtin-commands
		  %user-commands

		  add-command
		  show-commands
		  command-query)
  (import (scheme)
		  (melt data)
		  (melt lib console)
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
	  (gem-display (gem "[2C" "")
				(gem "[37;1m" (symbol->string (command-name command)))
				":\n"
				(gem "[5C" "")
				(gem "[38;5;166m" (command-desc command))
				"\n"))
	(do ((command-list (data-keys commands) (cdr command-list)))
		((null? command-list) #t)
	  (show-command (cdr (assq (car command-list) (data-cont commands))))))

  ;; query command in commadn-datas, return command ann command help procedure
  (define (command-query command command-datas)
	(if (memv command (data-keys command-datas))
		(command-proc (cdr (assq command (data-cont command-datas))))
		#f))

  ;; --------------------------------------------------------------------------------- ;;
  ;; ******************** commands insert ******************************************** ;;
  (import (melt command init))
  (import (melt command exec))
  
  (add-command init
			   %builtin-commands)
  (add-command exec
			   %builtin-commands)
  (add-command (make-command 'serve "A little server!" (lambda args
														 (display "This is a server!\n"))))
  
  )
