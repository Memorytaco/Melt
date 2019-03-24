(library (melt command exec)
  (export exec)
  (import (scheme)
          (melt structure)
		  (melt invoke)
		  (melt glob)
		  (melt lib console)
          (melt utils))
  
  (import type-command)
  ;; display the build command usage
  (define (exec-help)
    (gem-display (gem "[37;1m" "melt")
			  (gem "[38;5;67m" " exe")
			  (gem "[38;5;253m" " <chain-load> \n"))
    (gem-display "If the"
			  (gem "[38;5;253m" " <chain-load> ")
			  "is not provided, use"
			  (gem "[38;5;190m" " melt.scm ")
			  "instead. \n"))
  
  ;; the build command
  (define exec-cli
	(lambda args
	  (cond
	   [(null? args)
		(gem-display (gem "[37:1m" "please give me the ")
					 (gem "[38;5;253m" "melt.scm!\n"))]
	   [else
		(let ((chain-file (car args)))
		  (if (file-exists? chain-file)
			  (begin
				(load chain-file)
				(execute-chain %%chain))
			  (begin
				(format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename chain-file)))))])))

  (define exec
	(make-command 'exec
				  "melt command to execute one list actions, mostly build the site."
				  exec-cli))
  
  )
