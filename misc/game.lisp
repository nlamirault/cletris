;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          game.lisp
;;;; Purpose:       Launch a Cletris game
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************


(in-package :cl-user)

(load "./.quicklisp/setup.lisp")

(ql:quickload "cletris")
(cletris:cletris "user")
