;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; Copyright (C) 2014  Nicolas Lamirault

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package :cletris)



;; Tools


(setf (cl-log:log-manager)
      (make-instance 'cl-log:log-manager
                     :message-class 'cl-log:formatted-message))

(cl-log:start-messenger 'cl-log:text-file-messenger
                        :filename "/tmp/cletris.txt")


(defun extract-client-name (sequence)
  "Extract client name, ip, address, port and extra informations from a request
sended to the server."
  (let ((data (cl-ppcre:split ":" sequence)))
    (values (second data)
            (third data)
            (fourth data)
            (nthcdr 4 data))))


(defmacro with-socket-stream ((socket-stream) ip port &body body)
  "Macro which open a socket to IP:PORT and creates a SOCKET-STREAM.
After executing BODY, close the SOCKET-STREAM."
  `(let* ((server-socket (usocket:socket-connect ,ip ,port))
          (,socket-stream (usocket:socket-stream server-socket)))
     ,@body
     (finish-output ,socket-stream)
     (usocket:socket-close server-socket)))


(defmacro with-server-socket-stream ((socket-stream) socket &body body)
  "Creates a SOCKET-STREAM from the SOCKET and executes BODY."
  `(let* ((csock (usocket:socket-accept ,socket))
          (,socket-stream (usocket:socket-stream csock)))
     (when ,socket-stream
       ,@body
       (close ,socket-stream))))


;; ---------------
;; Network Entity
;; ---------------


(defclass networker ()
  ((name :initform nil
         :initarg :name
         :accessor networker-name)
   (ip :initform nil
       :initarg :ip
       :accessor networker-ip)
   (port :initform nil
         :initarg :port
         :accessor networker-port)
   (socket :initform nil
           :initarg :socket
           :accessor networker-socket)
   (process :initform nil
            :initarg :process
            :accessor networker-process))
  (:documentation "An entity for network operations."))


(defgeneric request-handler (networker)
  (:documentation "Handler for request messages."))


(defgeneric start-networker (networker)
  (:documentation "Start the networker and treat requests."))


(defmethod start-networker ((networker networker))
  (with-slots (name ip port socket process) networker
    (cl-log:log-message :info
                        (format nil "Start Networker ~A ~A:~A"name ip port))
    (setf socket (usocket:socket-listen ip port :backlog 4))
    (setf process
          (sb-thread:make-thread (lambda ()
                                   (request-handler networker))
                                 :name name))))


(defgeneric stop-networker (networker)
  (:documentation "Stop NETWORKER if it is running."))


(defmethod stop-networker ((networker networker))
  (with-slots (name ip port socket process) networker
    (cl-log:log-message :info
                        (format nil "Stop Networker ~A ~A:~A"name ip port))
    (when socket
      (usocket:socket-close socket))
    (when (sb-thread:thread-alive-p process)
      (sb-thread:terminate-thread process))))


;; --------------------
;; Cletris Game Server
;; --------------------


(defclass server (networker)
  ((clients :initform nil
            :initarg :clients
            :accessor server-clients))
  (:documentation "A new Cletris server."))


(defun make-server (ip port)
  "Creates a new Cletris game server."
  (make-instance 'server
                 :name "Cletris server"
                 :ip ip :port port :clients (make-hash-table :test #'equal)))


(defgeneric print-clients (server &optional stream)
  (:documentation "Print to STREAM all SERVER's clients."))


(defmethod print-clients ((server server) &optional (stream *standard-output*))
  (with-slots (clients) server
    (loop for name being the hash-key of clients
       as data = (gethash name clients)
       do (format stream "~&~A : ~A : ~A"
                  name (first data) (second data)))))


(defgeneric start-request (server)
  (:documentation "Make a request to all clients of SERVER to start game."))


(defmethod start-request ((server server))
  (with-slots (clients) server
    (loop for name being the hash-key of clients
       as data = (gethash name clients)
       do
         (cl-log:log-message :info
                             (format nil "Start Client ~A:~A start game"
                                     (first data) (second data)))
         (with-socket-stream (cstream) (first data) (parse-integer (second data))
           (format cstream "ready:~A:~A:~A" name (first data) (second data))))))


(defgeneric add-line-request (server name ip port lines)
  (:documentation "Send a request using informatiosn of CLIENT-DATA (a list
of (ip port state) to add some lines."))


(defmethod add-line-request ((server server) name ip port lines)
  (cl-log:log-message :info
                      (format nil "Server send add request ~A:~A:~A=~A"
                              name ip port lines))
  (with-socket-stream (cstream) ip (parse-integer port)
    (format cstream "add:~A:~A:~A:~A" name ip port lines)))


(defmethod request-handler ((server server))
  (with-slots (socket clients) server
    (loop
       (let* ((csock (usocket:socket-accept socket))
              (cstream (usocket:socket-stream csock)))
         (loop for line = (read-line cstream nil)
            until (null line)
            do
              (cl-log:log-message :info
                                  (format nil "Server received: ~A" line))
              (cond ((cl-ppcre:scan "new" line)
                     (multiple-value-bind (name ip port data)
                         (extract-client-name line)
                       (declare (ignore data))
                       (setf (gethash name clients) (list ip port 'ready))))
                    ((cl-ppcre:scan "del" line)
                      (multiple-value-bind (name ip port data)
                          (extract-client-name line)
                        (let ((names '())
                              (rs (make-random-state t)))
                          (maphash #'(lambda (key value)
                                       (unless (string-equal key name)
                                         (push key names)))
                                   clients)
                          (let* ((key (nth (random (length names) rs) names))
                                 (client-data (gethash key clients)))
                            (when client-data
                              (add-line-request server
                                                key
                                                (first client-data)
                                                (second client-data)
                                                (first data)))))))
                    ((cl-ppcre:scan "quit" line)
                     (multiple-value-bind (name ip port data)
                         (extract-client-name line)
                       (declare (ignore ip port data))
                       (remhash name clients)))
                    ))
         (close cstream)))))


;; --------------------
;; Cletris Game Client
;; --------------------


(defclass client (networker)
  ((server-ip :initform nil
              :initarg :server-ip
              :accessor client-server-ip)
   (server-port :initform nil
                :initarg :server-port
                :accessor client-server-port)
   (game :initform nil
         :initarg :game
         :accessor client-game))
  (:documentation "A new Tetris network client."))


(defun make-client (name ip port server-ip server-port game)
  "Creates a new Cletris client."
  (make-instance 'client
                 :name name :ip ip :port port
                 :server-ip server-ip :server-port server-port
                 :game game))


(defmethod request-handler ((client client))
  (with-slots (name ip port server-ip server-port socket) client
    (loop
       (let* ((csock (usocket:socket-accept socket))
              (cstream (usocket:socket-stream csock)))
         (when cstream
           (loop for line = (read-line cstream nil)
              until (null line)
              do
                (cl-log:log-message :info
                                    (format nil "Client received: ~A" line))
                (cond ((cl-ppcre:scan "hello" line)
                       (format t "~A:~A:~A ~A:~A"
                               name ip port server-ip server-port))
                      ((cl-ppcre:scan "ready" line)
                       (cletris name client))
                      ((cl-ppcre:scan "add" line)
                       (add-line (game-matrix (client-game client))))
                      )))
         (close cstream)))))


(defgeneric new-request (client)
  (:documentation "Send a request to a server find by IP and PORT to specify
add CLIENT to the network game."))


(defmethod new-request ((client client))
  (with-slots (name ip port server-ip server-port) client
    (cl-log:log-message :info
                        (format nil "Client ~A:~A send new request ~A:~A"
                                ip port server-ip server-port))
    (with-socket-stream (cstream) server-ip server-port
      (format cstream "new:~A:~A:~A" name ip port))))


(defgeneric delete-line-request (client lines)
  (:documentation "Send to the server that CLIENT have delete some lines"))


(defmethod delete-line-request ((client client) lines)
  (with-slots (name ip port server-ip server-port) client
    (cl-log:log-message :info
                        (format nil "Client delete lines ~A:~A ~A:~A = ~A"
                                ip port server-ip server-port lines))
    (with-socket-stream (cstream) server-ip server-port
      (format cstream "del:~A:~A:~A:~A" name ip port lines))))


(defgeneric quit-request (client)
  (:documentation "Send a request to the server to finish the game."))


(defmethod quit-request ((client client))
  (with-slots (name ip port server-ip server-port) client
    (cl-log:log-message :info
                        (format nil "Client ~A:~A send quit request ~A:~A"
                                ip port server-ip server-port))
    (with-socket-stream (cstream) server-ip server-port
      (format cstream "quit:~A:~A:~A" name ip port))))



;; Admin


(defclass network-game (game)
  ((client :initform nil
           :initarg :client
           :accessor game-client
           :documentation "The network client game."))
  (:documentation "A cletris networked game."))


(defmethod delete-line ((network-game network-game) number)
  (delete-line-request (game-client network-game) number))


(defun cletris-client (username ip port server-ip server-port)
  "Creates a new Cletris network client. Start it and add it to the server."
  (let ((client (make-client username ip (parse-integer port)
                             server-ip (parse-integer server-port)
                             (make-game))))
    (start-networker client)
    (new-request client)
    (cletris username client)
    client))


(defun cletris-server (ip port)
  "Creates a new cletris server. Start it and return this server."
  (let ((server (make-server ip (parse-integer port))))
    (start-networker server)
    server))
