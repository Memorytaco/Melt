;; TODO : unfinished file
(define-module (Flax command build)
  #:use-module (Flax site)
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

;; 
(define* (build #:key (config-file "config.scm"))
  (if (file-exists? (get-absolute-path config-file))
      (let ((obj (config-load config-file)))
	(if (is-site? obj)
	    (begin
	      (build-site obj)
	      (format #t "~%!!Done!!~%"))
	    (format (current-error-port) "Didn't receive site object !~%Last config expression must be site ~%")))
      (begin
	(format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename config-file)))))
