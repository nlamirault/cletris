;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cletris-test.asd
;;;; Purpose:       ASDF definition for Cletris unit tests
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cletris, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-user)
(defpackage cletris-test-asd
  (:use :cl :asdf))
(in-package :cletris-test-asd)

(defsystem cletris-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cletris
               :prove)
  :components ((:module "t"
                :components
                ((:file "package")
                 (:test-file "cletris" :depends-on ("package")))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)
                    (asdf:clear-system c)))
