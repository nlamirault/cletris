;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file for Cletris.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cletris, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cletris
  (:use :cl)
  (:documentation "A Tetris game.")
  (:export #:cletris

           #:*debug*
           ))

