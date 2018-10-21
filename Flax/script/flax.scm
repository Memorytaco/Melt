#!/usr/bin/guile --no-auto-compile
-*- scheme -*-
!#

;; command option
;; --no-auto-compile

;;Add modules here
(use-modules (Flax ui))

;; the main execution script
(apply flax (command-line))
