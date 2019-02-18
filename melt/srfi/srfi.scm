(library (melt srfi srfi)
  (export define-assoc-lambda)
  (import (scheme))
  
  
  ;; define a procedure receive assoc-list
  ;; it will return a procedure add an association arg list
  ;; layer on the procedure which is passed to this procedure
  ;; example:
  ;; (define (x y z) (display x) (display y))
  ;; (define another-x (define-assoc-lambda x '(y z) '("this is y\n" "this is z\n")))
  ;; (another-x) ==> this is y
  ;;                 this is z
  ;; (another-x '((y . "this is new y\n"))) ==> this is new y
  ;;                                            this is z
  (define define-assoc-lambda
    (lambda (procedure key-list default-value)
      (let ((default-assoc (map cons key-list default-value)))
        ;; return a procedure
        (lambda arguments
          (if (or (eq? arguments '())
                  (eq? (car arguments) '()))
              (apply procedure default-value)
              (apply procedure
                     (map cdr
                          ((lambda (assoc-list)
                             (do ((keys (reverse key-list) (cdr keys))
                                  (values (reverse default-value) (cdr values))
                                  (arg-list '()))
                                 ((eq? keys '()) arg-list)
                               (if (assq (car keys) assoc-list)
                                   (set! arg-list
                                         (cons (cons (car keys)
                                                     (cdr (assq (car keys) assoc-list)))
                                               arg-list))
                                   (set! arg-list
                                         (cons (cons (car keys)
                                                     (car values))
                                               arg-list)))))
                           (car arguments)))))))))

  )
