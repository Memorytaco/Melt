;; TODO : unfinished file
(define-module (Flax command build)
  #:use-module (Flax utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 r5rs)
  #:use-module (ice-9 match)
  
  #:export (show-help
	    build))

;; not complete
(define (show-help)
  (format #t "Useage:~%")
  (format #t "1.~%"))

(define* (build #:key (config-file "config.scm"))
  (if (eq? (basename config-file) "config.scm")
      (set! config-file (get-absolute-path config-file))
      (set! config-file (get-absolute-path config-file)))
  (values (if (file-exists? config-file)
	      (load config-file) ;; this function has some problem neet to dive into!! load procedure!! build procedure is fine!!
	      #f)
	  (basename config-file)))
