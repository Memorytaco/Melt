(define-module (Flax main)
               #:use-module (ice-9 format)
               #:use-module (ice-9 srfi-9)
               #:use-module (Flax subsite)
               #:use-module (Flax site)
               #:use-module (Flax utils)
               #:export (main))

(define main
  (lambda (arg . extra-args)
    (add-to-load-path (getcwd))
    ))
