;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Copyright (C) 2007, 2015  Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************



(in-package :asdf)


(defsystem cletris-network
  :name "cletris-network"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.3"
  :licence "MIT License"
  :description "A tetris game."
  :depends-on (:cletris :cl-ppcre :usocket :cl-log)
  :components
  ((:module :src
            :components
            ((:file "network")
             ))))
