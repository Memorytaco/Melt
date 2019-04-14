#!chezscheme
(library
  (melt version)
  (export show-version-history)
  (import (scheme)
          (melt cell)
          (melt lib console))

  (define (show-version-history)
    (do ((cur-list version-history (cdr cur-list)))
      ((null? cur-list) #t)
      (let ((ver (car cur-list)))
        (gem:format "Melt ~a --- ~a~%"
                    (list (begin (output-ver (car ver)) (output-ver))
                          (begin (output-desc (cdr ver)) (output-desc)))))))

  (define output-ver
    (make-cell (void)
               (lambda (str)
                 `("[37m" . ,str))))

  (define output-desc
    (make-cell (void)
               (lambda (str)
                 `("[38;5;220m" . ,str))))

  ;; there are three numbers in the version string X. Y. Z
  ;; -- the X is the Milestone , when it updates, this means
  ;;    it now is a new release and it may be not compatibale with
  ;;    previous release.
  ;; -- the Y is the stable release during X, add some functions or bug fix.
  ;; -- the Z is the trial number. Each tested feature will be in here.

  (define version-history
    '[("0.0.1" . "navigate to chezscheme!")
      ("0.0.2" . "add markdown lib! now it will work!")
      ("0.0.3" . "add markdown parser and sxml parser!")
      ("0.0.4" . "refine console and data, also add cell")
      ("0.0.5" . "refine sub command system, refine glob data")])

  )
