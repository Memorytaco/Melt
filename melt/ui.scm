#!chezscheme
(library (melt ui)
         (export melt)
         (import (scheme)
                 (melt utils)
				 (melt lib color)
				 (melt version)
                 (melt srfi match))

         (define (show-version)
           (display (string-append (ctext "[37;1m" "melt")
								   (ctext "[38;5;15m" " version ")
								   (ctext "[38;5;165m" "0.2.5")
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
					 (ctext "[38;5;111m" " -h -v -vs\n")))

		 ;; user interface
         (define (melt arg . extra-args)
		   (match extra-args
             [(or ("-h") ("--help"))
              (help)]
             [(or ("-v") ("--version")) 
              (show-version)]
             [(or ("-vs") ("--version-history"))
              (show-version-history)]
             (else (introduction)))

		   (cond
			[(file-exists? ".melt")
			 (cdisplay (ctext "[38;5;135m" "Now, Let's start! \n"))]
			[(file-exists? "melt.scm")
			 (load "melt.scm")]
			[else (cdisplay (ctext "[38;5;196m" "Error! ")
							(ctext "[38;5;222m" "melt configure file doesn't exist!\n"))])
		   
		   )

		 
		 
         )
