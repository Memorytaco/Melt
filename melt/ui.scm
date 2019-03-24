#!chezscheme
(library (melt ui)
         (export melt)
         (import (scheme)
                 (melt utils)
				 (melt lib console)
				 (melt version)
				 (melt glob)
				 (melt command)
				 (melt structure)
                 (melt srfi match))

         (define (show-version)

           (display (string-append (gem "[37;1m" "melt")
								   (gem "[38;5;15m" " version ")
								   (gem "[38;5;165m" "0.0.3")
								   "\n")))

		 ;; the basic information
         (define (introduction)
           (gem-display (gem "[37m" "This is melt! Meta Excellent Local Note System.\n")
					 (gem "[37m" "Please use \"-h\" or \"--help\" to get further help.\nFor more information please follow")
					 (gem "[36m" " github io page.\n")))		 

         ;; basic help information
         (define (help)
           (gem-display (gem "[37;1m" "melt ")
						(gem "[38;5;102m" "[options] [command] [command options] \n"))
		   (gem-display (gem "[38;5;80m" "available options are :")
						(gem "[38;5;111m" " -h | -v | -vs | -l\n")))

		 ;; to structure commands
		 (define (prepare flag)
		   (define (melt-load)
			 (if (file-exists? ".melt")
				 (cond
				  [(file-directory? ".melt")
				   (load ".melt/settings.scm")
				   #t]
				  [else #f])
				 #f))
		   
		   (if flag
			   (if (melt-load)
				   (begin
					 (gem-display (gem "[38;5;10m" "==========")
							   (gem "[38;5;142m" "  Available commands :\n"))
					 (show-commands %builtin-commands)				 
					 (show-commands %user-commands))
				   (begin
					 (gem-display (gem "[38;5;196m" "Error! ")
							   (gem "[38;5;222m" "melt configure file doesn't exist! Only show builtin commands\n"))
					 (gem-display (gem "[38;5;10m" "==========")
							   (gem "[38;5;142m" "  Available commands :\n"))
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
			  (prepare #t)
			  (exit 0)]
             (else (if (null? extra-args)
					   (begin (introduction)
							  (exit 0)))))
           (if (not (file-exists? ".melt"))
			   (gem-display (gem "[38;5;196m" "You are not in the root of working directory!\n")))
		   (prepare #f)
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
			   (gem-display (gem "[38;5;99m" "Command not available!\n"))
			   (prepare #t)]))
		   )

		 
		 
         )
