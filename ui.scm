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
  (format #t "Please add \"-h\" or \"--help\" to get further help~%")
  (format #t "For more information please follow github io page~%"))

(define (show-flax-help)
  (format #t "Basic usage : ~%")
  (format #t "flax [ command ] [ options ] [ arguments ] ~%~%")
  (format #t "command list as follows :: ~%")
  (format #t "        build~%"))




;; The main function
(define (flax arg0 . extra-args)
  (add-to-load-path (getcwd))
  (match extra-args
    (() (show-flax))
    ((or ("-h") ("--help"))
     (show-flax-help))
    (("build")
     (build))
    (("build" args ...)
     (build #:config-file (car args)))
    (("serve")
     (format #t "Not ready now, Sorry!~%"))
    (_ (show-flax))))
