
(library (net)
  (export dial-tcp dial-udp serve)
  (import (chezscheme) (fn))

  (define __ (load-shared-object "libnet.so"))

  (define-ftype strerror-f (function (int) string))

  (define-ftype fd-result
    (struct
      [fd int]
      [errval int]
      [strerror (* strerror-f)]))

  (define (new-fd-result)
    (make-ftype-pointer fd-result (foreign-alloc (ftype-sizeof fd-result))))

  (define (fd-result-fields/free fdr)
    (let ([fd (ftype-ref fd-result (fd) fdr)]
          [errval (ftype-ref fd-result (errval) fdr)]
          [strerror (ftype-ref fd-result (strerror) fdr)])
      (foreign-free fdr)
      (values fd errval strerror)))

  (define (ftype-free ft)
    (foreign-free (ftype-pointer-address ft)))

  (define (valid-port? port)
    (unless (or (string? port)
                (and (integer? port)
                     (exact? port)
                     (< 0 port 35565)))
      (errorf 'dial "port must be a string or exact integer between 0 and 35565: ~a" port)))

  (define ip:tcp (foreign-ref 'int (foreign-entry "ipproto_tcp") 0))
  (define ip:udp (foreign-ref 'int (foreign-entry "ipproto_udp") 0))

  (define c:dial
    (foreign-procedure "c_dial" (int string string (* fd-result)) (* fd-result)))

  (define c:listen
    (foreign-procedure "c_listen" (int string string (* fd-result)) (* fd-result)))

  (define c:accept
    (foreign-procedure "c_accept" (int) (* fd-result)))

  (define dial-tcp
    (fn ([host :: string?]
         [port :: valid-port?]
         [transcoder :: (one-of #f transcoder?) = #f])
      (dial ip:tcp host port transcoder)))

  (define dial-udp
    (fn ([host :: string?]
         [port :: valid-port?]
         [transcoder :: (one-of #f transcoder?) = #f])
      (dial ip:udp host port transcoder)))

  (define (dial proto host port transcoder)
    (if (number? port) (set! port (number->string port)))
    (let-values ([(fd errval strerror) (fd-result-fields/free (c:dial proto host port (new-fd-result)))])
      (if (> 0 fd)
          (raise (condition (make-who-condition 'dial) (make-i/o-error) (make-message-condition (strerror errval)))))
      (open-fd-input/output-port fd 'none transcoder)))

  (define serve
    (case-lambda
      [(proto host port fn) (serve proto host port fn #f)]
      [(proto host port fn transcoder)
       (let* ([fdr (c:listen proto host port (new-fd-result))]
              [fd (ftype-ref fd-result (fd) fdr)]
              [errval (ftype-ref fd-result (errval) fdr)]
              [strerror (ftype-ref strerror-f () (ftype-ref fd-result (strerror) fdr))])
         (if (> 0 fd)
             (raise (condition (make-who-condition 'serve) (make-i/o-error) (make-message-condition (strerror errval)))))
         (call-with-port (open-fd-input/output-port fd 'none transcoder) fn))]))

  (define (accept-forever fn)
    (lambda (fd transcoder)
      (let* ([fdr (c:accept fd (new-fd-result))])
        (void))))

)
