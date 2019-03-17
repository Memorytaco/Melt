#!chezscheme
(library (melt lib console)
  (export special-sequence
		  gem-display
		  gem-command
		  gem)
  (import (scheme))

  ;; apply one shell sequence, and it won't reset it
  (define (gem-command shell-sequence)
	(string-append (string #\033)
					shell-sequence))

  ;; generate code sequence
  (define (gem shell-sequence . text)
	(string-append (gem-command shell-sequence)
				   (if (null? text)
					   ""
					   (car text))
				   (gem-command "[0m")))

  ;; just append strings and display it
  (define gem-display
	(lambda args
	  (display (apply string-append args))))

  ;; some special sequence
  (module special-sequence
		  [g-white
		   gem-reset
		   g-bar]

		  (define g-bar "█")
		  (define g-white "[37m")
		  (define gem-reset "[0m"))

  )
