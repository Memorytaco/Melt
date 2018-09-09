#!/usr/bin/guile --no-auto-compile
-*- scheme -*-
!#

(use-modules (ice-9 match))

(define (Hello name)
    (format #t "Hello ~a\n" name))

(Hello (match (command-line)
              (('Flax object) object)
              ((Head tail) tail)))
