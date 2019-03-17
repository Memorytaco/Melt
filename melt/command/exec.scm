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
			  (gem "[38;5;253m" " <user-file> \n"))
    (gem-display "If the"
			  (gem "[38;5;253m" " <user-file> ")
			  "is not provided, use"
			  (gem "[38;5;190m" " melt.scm ")
			  "instead. \n"))
  
  ;; the build command
  (define (exec-cli user-file)
    (if (file-exists? user-file)
        (begin
		  (load user-file)
		  (execute-chain %%chain))
		(begin
          (format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename user-file)))))

  (define exec
	(make-command 'exec
				  "melt command to execute one list actions, mostly build the site."
				  exec-cli))
  
  )
