(import (melt structure))
(import type-chain)
(import (melt data))
(import (melt invoke))

(define x (init-chain #t (lambda () (display "hello")) (create-data)))
(add-hook! x (create-hook 'text2 'proc `(,(lambda () (display "\nncie")))))
(add-hook! x (create-hook 'test1 'data (create-data '(x) '(2))))





