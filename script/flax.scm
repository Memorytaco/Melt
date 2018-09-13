#!/usr/bin/guile 
-*- scheme -*-
!#

;; command option
;; --no-auto-compile


;; NOTE: for test
(add-to-load-path 
    "/home/eilliot/Lasga/Repository/Local_Repository/Guile/Dev")
;;Add modules here
(use-modules (Flax ui))

;; the main execution script
;; TODO : replace it with my main function
(flax (command-line))
