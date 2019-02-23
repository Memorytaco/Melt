#!chezscheme
(library (melt ui)
         (export melt)
         (import (scheme)
                 (melt utils)
				 (melt lib color)
				 (melt version)
				 (melt command)
				 (melt structure)
                 (melt srfi match))

         (define (show-version)
           (display (string-append (ctext "[37;1m" "melt")
								   (ctext "[38;5;15m" " version ")
								   (ctext "[38;5;165m" "0.1.6")
								   "\n")))

		 ;; the basic information
         (define (introduction)
           (cdisplay (ctext "[37m" "This is melt! Meta Excellent Local Note System.\n")
					 (ctext "[37m" "Please use \"-h\" or \"--help\" to get further help.\nFor more information please follow")
					 (ctext "[36m" " github io page.\n")))		 

         ;; basic help information
         (define (help)
           (cdisplay (ctext "[37;1m" "melt ")
					 (ctext "[38;5;102m" "[options] [command] [command options] \n"))
		   (cdisplay (ctext "[38;5;80m" "available options are :")
					 (ctext "[38;5;111m" " -h | -v | -vs | -l\n")))

		 ;; to structure commands
		 (define (welcome flag)
		   (define (melt-load)
			 (cond
			  [(file-exists? ".melt")
			   (load ".melt")
			   #t]
			  [(file-exists? "melt.scm")
			   (load "melt.scm")
			   #t]
			  [else #f]))
		   
		   (if flag
			   (if (melt-load)
				   (begin
					 (cdisplay (ctext "[38;5;10m" "==========")
							   (ctext "[38;5;142m" "  Available commands :\n"))
					 (show-commands %builtin-commands)				 
					 (show-commands %user-commands))
				   (begin
					 (cdisplay (ctext "[38;5;196m" "Error! ")
							   (ctext "[38;5;222m" "melt configure file doesn't exist! Only show builtin commands\n"))
					 (cdisplay (ctext "[38;5;10m" "==========")
							   (ctext "[38;5;142m" "  Available commands :\n"))
					 (show-commands %builtin-commands)))
			   (melt-load)))

		 ;; user interface
         (define (melt arg . extra-args)
		   (match extra-args
             [(or ("-h") ("--help"))
              (help)
			  (exit 0)]
             [(or ("-v") ("--version")) 
              (show-version)
			  (exit 0)]
             [(or ("-vs") ("--version-history"))
              (show-version-history)
			  (exit 0)]
			 [(or ("-l") ("--list"))
			  (welcome #t)
			  (exit 0)]
             (else (if (null? extra-args)
					   (begin (introduction)
							  (exit 0))
					   (welcome #f))))
		   (let ((command-built (command-query (string->symbol (car extra-args))
											   %builtin-commands))
				 (command-user (command-query (string->symbol (car extra-args))
											  %user-commands)))
			 (cond
			  [command-built
			   (apply command-built
					  (cdr extra-args))]
			  [command-user
			   (apply command-user
					  (cdr extra-args))]
			  [else
			   (cdisplay (ctext "[38;5;99m" "Command not available!\n"))
			   (welcome #t)]))
		   )

		 
		 
         )
