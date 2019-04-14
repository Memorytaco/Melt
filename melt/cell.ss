(library
  (melt cell)
  (export make-cell)
  (import (scheme))

  (define (%check-cell-proc proc)
    (if (eq? 2 (abs (procedure-arity-mask proc)))
        proc
        (error 'make-cell "incorrect number of arguments of cell filter definition")))

  (define (%inner-proc proc)
    (let ((internal (%check-cell-proc proc)))
      (case-lambda
        [(value)
         (if (eq? 2 (abs (procedure-arity-mask value)))
             (set! internal value)
             (error 'make-cell "incorrect number of arguments of cell filter definition"))]
        [() internal])))

  (define make-cell
    (case-lambda
      [(default-value)
       (make-cell default-value (lambda (x) x))]
      [(default-value filter)
       (let ((internal-value default-value)
             (internal-cell (%inner-proc filter)))
         (case-lambda
           [(value)
            (set! internal-value value)]
           [(value upt-filter)
            (set! internal-value value)
            (internal-cell upt-filter)]
           [()  ((internal-cell) internal-value)]))]))

  )
