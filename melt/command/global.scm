(library (melt command global)
  (export %builtin-commands
		  %user-commands

		  add-command)
  (import (scheme)
		  (melt data)		  
		  (melt structure))

  (import type-command)

  (define %builtin-commands (create-data))

  (define %user-commands (create-data))

  (define (add-command command . command-data )
	(update-data!  (if (null? command-data)
					   %user-commands
					   (car command-data))
				   (create-data (list (command-name command))
								(list command))))

  (define (remove-command command . command-data)
	(update-data! (if (null? command-data)
					   %user-commands
					   (car command-data))
				  (list (command-name command))))

;  (import (melt command build))
;  (add-command build $builtin-commands)


  )
