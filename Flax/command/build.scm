(define-module (Flax command build)
  #:use-module (Flax site)
  #:use-module (Flax utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 r5rs)
  #:use-module (ice-9 match)
  
  #:export (show-build-help
	    build))

;; display the build command usage
(define (show-build-help)
  (format #t "Useage: flax build <ConfigFileName> ~%")
  (format #t "If the <ConfigFileName> is not provided, use \"config.scm\" instead. ~%"))

;; the build command which is invoked by the ui
(define* (build #:key (config-file "config.scm"))
  (if (file-exists? (get-absolute-path config-file))
      (let ((obj (config-load config-file)))
	(if (is-site? obj)
	    (build-site obj)
	    (format (current-error-port) "Didn't receive site object !~%Last config expression must be site ~%")))
      (begin
	(format #t "Coundn't find config file !! ~%expect ~a but got nothing !~%" (basename config-file)))))
