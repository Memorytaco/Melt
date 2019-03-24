#!chezscheme
(library (melt version)
  (export show-version-history)
  (import (scheme)
		  (melt lib console))

  (define (show-version-history)
	(do ((cur-list version-history (cdr cur-list)))
		((null? cur-list) #t)
	  (let ((ver (car cur-list)))
		(format #t (string-append "Melt " (gem "[37m" (car ver)) " --- " (gem "[38;5;220m" (cdr ver)) "~%")))))

  ;; there are three numbers in the version string X. Y. Z
  ;; -- the X is the Milestone , when it updates, this means
  ;;    it now is a new release and it may be not compatibale with
  ;;    previous release.
  ;; -- the Y is the stable release during X, add some functions or bug fix.
  ;; -- the Z is the trial number. Each tested feature will be in here.
  (define version-history
    '[("0.0.1" . "navigate to chezscheme!")
	  ("0.0.2" . "add markdown lib! now it will work!")
	  ("0.0.3" . "add markdown parser and sxml parser!")])

  
  )
