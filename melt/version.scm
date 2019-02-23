#!chezscheme
(library (melt version)
  (export show-version-history)
  (import (scheme)
		  (melt lib color))

  (define (show-version-history)
	(do ((cur-list version-history (cdr cur-list)))
		((null? cur-list) #t)
	  (let ((ver (car cur-list)))
		(format #t (string-append "Melt " (ctext "[37m" (car ver)) " --- " (ctext "[38;5;220m" (cdr ver)) "~%")))))

  ;; there are three numbers in the version string
  ;; -- the first is the main version number, when it updates, this means
  ;;    melt can be packaged and can run independently. melt now is not
  ;;    compatibal with previous main version.
  ;; -- the second number is the minor version number, when it updates,
  ;;    melt is stable with all branch version. melt can be patched
  ;;    and distributed.
  ;;    melt is compatibal with previous second version.
  ;; -- the third number is the branch version number. Every bug fix or feature
  ;;    added or some procedure updated will update the number.
  (define version-history
    '[("0.0.1" . "complete the basic functions, split the process procedure out.")
      ("0.0.2" . "refine the site procedure, refine the page procedure adding file extension support.")
      ("0.0.3" . "refine the write-content procedure, let the logic be better understood!!")
      ("0.0.4" . "add markdown support and fix some bugs!")
      ("0.0.5" . "successfully navigate the utils and ui!")
      ("0.0.6" . "add match support!! add srfi !! Will do it all the time!")
      ("0.0.7" . "get defaults.scm done! get sxml->html done! Now left post.scm site.scm page.scm space.scm command builder.")
      ("0.0.8" . "complete post.scm and page.scm comming up with some extra utilities in utils.scm.")
      ("0.0.9" . "get the asset.scm done! fix string-trim procedure and add some usefull utilities.")
      ("0.0.10" . "has defined hook system. Although the system is really simple. need to renegrate all logic.")
      ("0.0.11" . "has refined the hook system. add some procedures.")
      ("0.0.12" . "now it can work fine! except lacking markdown support. Latter will refine it again!")
      ("0.1.0" . "navigate to chezscheme!")
      ("0.1.1" . "adding new feature, refactor the whole program again!")
	  ("0.1.2" . "add data structure and refine hook system!")
	  ("0.1.3" . "temp version, refactor the whole structure!")
	  ("0.1.4" . "change flax to melt.")
	  ("0.1.5" . "add commandline color! Cool!")
	  ("0.1.6" . "add subcommand system!")
	  ("0.1.7" . "complete subcommand logic and modify structure!")])

  
  )
