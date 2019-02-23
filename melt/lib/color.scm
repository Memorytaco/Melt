#!chezscheme
(library (melt lib color)
  (export color-sequence
		  cdisplay
		  gemcolor
		  ctext
		  bar)
  (import (scheme))

  (define bar "█")

  (define (gemcolor color-sequence)
	(string-append (string #\033)
				   color-sequence))

  (define (ctext color-sequence text)
	(string-append (gemcolor color-sequence)
				   text
				   (gemcolor "[0m")))

  (define cdisplay
	(lambda args
	  (display (apply string-append args))))
  
  (module color-sequence
   [white
	color-reset]

   (define white "[37m")
   (define color-reset "[0m"))

  )
