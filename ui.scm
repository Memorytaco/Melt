;; NOTE : unfinished file
(define-module (Flax ui)
  #:use-module (Flax utils)
  #:use-module (Flax command build)
  
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  
  #:export (flax))

;; varibles
(define %commands '("build" "serve"))


;; TODO : continue to update
(define (show-flax)
  (format #t "This is another static site generator~%")
  (format #t "Please add \"-h\" or \"--help\" to get further help~%"))

(define (show-flax-help)
  (format #t "Ok~%"))

;; The main function
(define (flax arg0 . extra-args)
  (add-to-load-path (getcwd))
  (match extra-args
    (() (show-flax))
    ((or ("-h") ("--help"))
     (show-flax-help))
    (("build")
     (let-values (((flag config-file) (build)))
       (if (not flag)
	   (format (current-error-port) "~a : Config File Doesn't exist!!~%" config-file))))
    (("build" args ...)
     (let-values (((flag config-file) (build #:config-file (car args))))
       (if (not flag)
	   (format (current-error-port) "~a : Config File Doesn't exist!!~%" config-file))))
    (_ (show-flax))))
