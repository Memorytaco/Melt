(define-module (Flax ui)
  #:use-module (Flax utils)
  #:use-module (Flax command build)
  
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  
  #:export (flax))

;; command list
(define %commands% '("build" "serve"))

;; there are three numbers in the version string
;; -- the first is the main version number, when it updates, this means
;;    Flax can be packaged and can run independently. Flax now is not
;;    compatibal with previous main version.
;; -- the second number is the minor version number, when it updates,
;;    Flax is stable with all branch version. Flax can be patched
;;    and distributed.
;;    Flax is compatibal with previous second version.
;; -- the third number is the branch version number. Every bug fix or feature
;;    added or some procedure updated will update the number.
(define (show-version)
  (format #t "Flax version 0.0.4 ~%"))

(define --version-history--
  '("Flax 0.0.1 ---  complete the basic functions, split the process procedure out ~%"
    "Flax 0.0.2 ---  refine the site procedure, refine the page procedure adding file extension support ~%"
    "Flax 0.0.3 ---  refine the write-content procedure, let the logic be better understood!! ~%"
    "Flax 0.0.4 ---  add markdown support and fix some bugs! ~%"))

;; the basic information
(define (show-flax)
  (format #t "This is just another static site generator~%")
  (format #t "Please add \"-h\" or \"--help\" to get further help~%")
  (format #t "For more information please follow github io page~%"))

;; basic help information
(define (show-flax-help)
  (format #t "Basic usage : ~%")
  (format #t "flax [ command ] [ options ] [ arguments ] ~%~%")
  (format #t "command list as follows :: ~%")
  (format #t "        build~%")
  (format #t "basic information argument:~%")
  (format #t "[ -v || --version ]  version number~%"))


;; The main function
(define (flax arg0 . extra-args)
  (add-to-load-path (getcwd))
  (match extra-args
    (() (show-flax))
    ((or ("-h") ("--help"))
     (show-flax-help))
    ((or ("-v") ("--version"))
     (show-version))
    (("build")
     (build))
    (("build" (or "-h" "--help"))
     (show-build-help))
    (("build" args ...)
     (build #:config-file (car args)))
    (("serve")
     (format #t "Not ready now, Sorry!~%"))
    (_ (show-flax))))
