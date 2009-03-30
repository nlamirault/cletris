;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cletris-network.asd
;;;; Purpose:       ASDF definition for Cletris in network.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cletris, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cletris-network
  :name "cletris-network"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.3"
  :licence "Lisp Lesser GNU General Public License"
  :description "A tetris game."
  :depends-on (:cletris :cl-ppcre :usocket :cl-log)
  :components
  ((:module :src
            :components
            ((:file "network")
             ))))


