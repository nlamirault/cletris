;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Some specials variables of the Cletris game
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cletris, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cletris users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cletris)


(defparameter *version* (asdf:component-version (asdf:find-system "cletris")))

(defparameter *debug* nil "When T, active some logs for debugging.")

(defparameter *cletris-directory*
  (namestring (asdf:component-relative-pathname (asdf:find-system :cletris)))
  "Directory with contains CLETRIS source files.")

(defparameter *speed* 10)

(defparameter *scores* '() "The best scores.")
                                        

;; screen

(unless (boundp '+width+)
  (defconstant +width+ 640 "The default width."))


(unless (boundp '+height+)
  (defconstant +height+ 480 "The default height."))


(unless (boundp '+game-left-corner-x+)
  (defconstant +game-left-corner-x+ 250))


(unless (boundp '+game-left-corner-y+)
  (defconstant +game-left-corner-y+ 50))


(unless (boundp '+blocks+)
  (defconstant +blocks+
    '((((0 . -1) (0 . 0) (0 . 1) (1 . 1)) 255 255 255)          ;; J
      (((0 . -1) (0 . 0) (0 . 1) (-1 . 1)) 255 0 255)           ;; L
      (((0 . -1) (0 . 0) (-1 . 0) (1 . 0)) 178 109 57)          ;; T
      (((0 . -2) (0 . -1) (0 . 0) (0 . 1)) 255 0 0)             ;; I
      (((0 . -1) (1 . -1) (1 . 0) (0 . 0)) 0 0 255)             ;; O
      (((-1 . -1) (-1 . 0) (0 . 0) (0 . 1)) 0 255 0)            ;; S
      (((1 . -1) (1 . 0) (0 . 0) (0 . 1)) 0 255 255))))         ;; Z  


(unless (boundp '+matrix-background+)
  (defconstant +matrix-background+ '(50 50 50)))


;; multimedia files

(unless (boundp '+background+)
  (defconstant +background+ "img/background.png"))


;; red : 255 0 0
;; gren : 0 255 0
;; cyan : 0 255 255
;; white : 255 255 255
;; brown : 178 109 57
;; blue : 0 0 255
;; magenta : 255 0 255