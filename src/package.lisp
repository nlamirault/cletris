;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Copyright (C) 2014, 2015  Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(defpackage :cletris
  (:use :cl)
  (:documentation "A Tetris game.")
  (:export #:cletris

           #:*debug*
           ))
