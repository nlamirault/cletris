;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtests.lisp
;;;; Purpose:       Climon unit tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************


(in-package :cl-user)

;; (let ((quicklisp-file
;;        (make-pathname :directory (pathname-directory (user-homedir-pathname))
;; 		      :name "quicklisp/setup" :type "lisp")))
;;   (format t "Quiclisp: ~s" quicklisp-file)
;;   (load quicklisp-file))
;;(load "./.quicklisp/setup.lisp")

(ql:quickload "cletris")
(ql:quickload "cletris-test")

(setq lisp-unit:*print-failures* t)
(setq lisp-unit:*print-errors* t)
(setq lisp-unit:*print-summary* t)
