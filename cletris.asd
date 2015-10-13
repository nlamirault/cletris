;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Copyright (C) 2014, 2015  Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :asdf)



(defsystem cletris
  :name "cletris"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.9.0"
  :licence "MIT License"
  :description "A tetris game."
  :depends-on (:pal :cl-ppcre) ;; :usocket :cl-log)
  :components
  ((:module :src
            :components
            ((:file "package")
             (:file "specials" :depends-on ("package"))
             (:file "cletris" :depends-on ("specials"))
             ;;(:file "network" :depends-on ("cletris"))))))
             ))))
