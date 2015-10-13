;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cletris.lisp
;;;; Purpose:       Cletris unit tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cletris, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(in-package :cletris-test)


(plan 2)

(is (format nil "~A/.config/cletris"
            (sb-ext:posix-getenv "HOME"))
    (cletris::get-cletris-directory))


(is nil (cletris::read-scores))

(finalize)
