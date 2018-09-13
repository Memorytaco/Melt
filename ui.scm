(define-module (Flax ui)
               #:use-module (ice-9 format)
               #:use-module (srfi srfi-9)
               #:use-module (ice-9 match)

               #:export (flax))

;; NOTE: varibles 
(define %commands '("build" "serve"))


;; TODO : continue to update
(define (show-flax)
  (format #t "This is another static site generator~%")
  (format #t "Please add \"-h\" or \"--help\" to get further help~%"))

(define (show-flax-help)
  (format #t "Ok~%"))


(define flax
  (lambda (arg0 . extra-args)
  ;; add current directory to load path
    (add-to-load-path (getcwd))
    (match extra-args
      (() (show-flax))
      ((or ("-h") ("--help"))
        (show-flax-help))
      (_  (show-flax)))))
