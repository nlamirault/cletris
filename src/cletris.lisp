

(in-package :cletris)


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+:sbcl
  (setf *random-state* (make-random-state t)))

;; From Hunchentoot
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))


(defun get-cletris-directory ()
  "Get the home directory of Cletris."
  (let ((directory (concatenate 'string
                                (sb-ext:posix-getenv "HOME")
                                "/"
                                ".cletris/")))
    (ensure-directories-exist directory)))


(defun read-scores ()
  "Load the best scores."
  (with-open-file (stream (concatenate 'string
                                       (get-cletris-directory)
                                       "scores.txt")
                          :direction :input)
      (loop for line = (read-line stream nil nil)
         until (null line)
         as data = (cl-ppcre:split ";" line)
         collect (list (parse-integer (first data))
                       (second data)
                       (third data)))))


(defun write-scores (scores)
  "Write the best scores."
  (with-open-file (os (concatenate 'string
                                   (get-cletris-directory)
                                   "scores.txt")
                      :direction :output
                      :if-exists :overwrite)
      (loop for data in scores
         do (format os "~A;~A;~A~%"
                    (first data) (second data) (third data)))))


(defun update-scores (username points)
  "Update the score's file. Add POINTS of USERNAME in the score list.
Return a list of the 10 best scores."
  (let* ((scores (read-scores))
         (sorted (sort (push (list points username (iso-time))
                             scores)
                       #'> :key #'first)))
    (write-scores (if (> (length sorted) 10)
                      (subseq sorted 0 10)
                      sorted))))


(if *cletris-directory*
      (pal:define-tags background
                       (pal:load-image (concatenate 'string
                                                    *cletris-directory*
                                                    +background+)
                                       t))
      (error "CLETRIS directory isn't defined : *CLETRIS-DIRECTORY*"))


(defclass tetris ()
  ((image :accessor tetris-image :initarg :image)))


(defclass background (tetris)
  ()
  (:default-initargs :image (pal:tag 'background)))


(defgeneric draw (tetris)
  (:documentation "Draw a Tetris game."))


(defmethod draw ((s tetris))
  (when *debug*
    (format t "~&Draw ~A" (tetris-image s)))
  (pal:draw-image (tetris-image s) (pal:v 0 0)))


(defun random-elt (rs sequence)
  (nth (random (length sequence) rs) sequence))


(defun get-block (rs)
  "Get a new random colored block using the random state RS."
  (copy-tree (random-elt rs +blocks+)))


(defun init-matrix ()
  "Creates a new matrix."
  (make-array (list 20 10)))


(defun update-matrix (matrix block block-x block-y)
  "Update the matrix saving the current block."
  (loop for part in (car block)
     as x = (car part)
     as y = (cdr part)
     when (and (>= (+ x block-x) 0)
               (>= (+ y block-y) 0))
     do (setf (aref matrix (+ y block-y) (+ x block-x))
              (cdr block)))
  (when *debug*
    (format t "NEW Tetris game :~&~A" matrix)))


(defun draw-square (x y r g b)
  "Draw a square from (X,Y) with color defined by R G B codes."
  (pal:draw-polygon (list (pal:v x y)
                          (pal:v (+ x 18) y)
                          (pal:v (+ x 18) (+ y 18))
                          (pal:v x (+ y 18)))
                    r g b
                    255))


(defun draw-block (block x y)
  "Print a block on (x,y) coordonates."
  (loop for part in (car block)
     as x0 = (+ (* (+ x (car part)) 20) +game-left-corner-x+)
     as y0 = (+ (* (+ y (cdr part)) 20) +game-left-corner-y+)
     as r = (first (cdr block))
     as g = (second (cdr block))
     as b = (third (cdr block))
     do (draw-square x0 y0 r g b)))


(defun draw-game (matrix block next-block block-x block-y lines level points
                  &optional username)
  "Draw the MATRIX game, old blocks, current BLOCK and
the NEXT-BLOCK. LINES and LEVEL is for the user."
  (pal:clear-screen (pal:color 0 0 0))
  (when matrix
    (loop for y from 0 to 19
       as y0 = (+ (* y 20) +game-left-corner-y+)
       do (loop for x from 0 to 9
             as x0 = (+ (* x 20) +game-left-corner-x+)
             do (let (r g b)
                  (if (listp (aref matrix y x))
                      (setf r (first (aref matrix y x))
                            g (second (aref matrix y x))
                            b (third (aref matrix y x)))
                      (setf r (first +matrix-background+)
                            g (second +matrix-background+)
                            b (third +matrix-background+)))
                  (draw-square x0 y0 r g b)))))
  (pal:draw-text (format nil "Level ~A" level) (pal:v 15 20))
  (pal:draw-text (format nil "Lines ~A" lines) (pal:v 15 65))
  (pal:draw-text (format nil "Points ~A" points) (pal:v 15 105))
  (when username
    (pal:draw-text (format nil "~A" username) (pal:v 15 145)))
  (when block
    (draw-block block block-x block-y))
  (when next-block
    (pal:draw-text "Next" (pal:v 500 110))
    (draw-block next-block 15 8)))


(defun in-area-p (matrix block block-x block-y key)
  "Check if the current BLOCK is in the area.
KEY is :left or :right."
  (let ((in t))
    (loop for part in (car block)
       as x = (+ block-x (car part))
       as y = (+ block-y (cdr part))
       do
         (when *debug*
           (format t "~&Part of Block ~&~A : ~A ~A = ~A ~A"
                   block block-x block-y x y))
         (unless (and (< (incf y) 20)
                      (or (< y 0)
                          (> x 9)
                          (< x 0)
                          (not (listp (aref matrix y x))))
                      (cond ((equal :left key)
                             (let ((new-x (1- x)))
                               (and (>= new-x 0)
                                    (not (listp (aref matrix y new-x))))))
                            ((equal :right key)
                             (let ((new-x (1+ x)))
                               (and (<= new-x 9)
                                    (not (listp (aref matrix y new-x))))))
                            (t t)))
           (setf in nil)))
    in))


(defun rotate (matrix block block-x block-y)
  "Rotate the BLOCK if the new rotated block is in the game area."
  (let ((new-block (copy-tree block)))
    (loop for part in (car new-block)
       as x = (car part)
       as y = (cdr part)
       do (setf (car part) y
                (cdr part) (- x)))
    (let ((left (in-area-p matrix new-block block-x block-y :left))
          (right (in-area-p matrix new-block block-x block-y :right)))
      (when *debug*
        (format t "~&Left : ~A~&Right : ~A" left right))
      (if (and left right)
          new-block
          block))))


(defun clean-matrix (matrix)
  "Search filled lines into MATRIX and delete them.
Return number of fulled lines deleted."
  (let ((lines 0))
    (loop for y from 0 to 19
       as fulled = t
       do
         (loop for x from 0 to 9
            when (not (listp (aref matrix y x)))
            do (setf fulled nil))
         (when fulled
           (incf lines)
           (loop for z from y downto 1
              do (loop for x from 0 to 9
                    do (setf (aref matrix z x) (aref matrix (1- z) x)
                             (aref matrix (1- z) x) 0)))))
    lines))


(defun add-line (matrix)
  "Add a line which contains a few blocks to MATRIX."
  (show-matrix matrix)
  (loop for y from 1 to 19
     do (loop for x from 0 to 9
           do (setf (aref matrix (1- y) x) (aref matrix y x))))
  (let ((y 19)
        (rs (make-random-state t)))
    (loop for i from 0 to 9
       as r = (random 3 rs)
       do (setf (aref matrix y i)
                (if (> r 0)
                    (cdr (get-block rs))
                    0))))
  (pal::update-screen)
  (show-matrix matrix))


(defun show-matrix (matrix &optional (stream *standard-output*))
  "Print to STREAM the MATRIX."
  (when matrix
    (format stream "~&-----------------------~%")
    (loop for y from 0 to 19
       do
         (format stream "| ")
         (loop for x from 0 to 9
            do (if (listp (aref matrix y x))
                   (format stream "~A " T)
                   (format stream "~A " " ")))
         (format stream "|~%"))
    (format stream "~&-----------------------~%")))


(defun display-about ()
  "Show about informations if no games running."
  (pal:clear-screen (pal:color 0 0 0))
  (pal:draw-text (format nil "Cletris ~A" *version*) (pal:v 100 150))
  (pal:draw-text "Author :" (pal:v 100 200))
  (pal:draw-text "Nicolas Lamirault" (pal:v 100 250))
  (pal::update-screen)
  (sleep 3))


(defun display-scores ()
  "Show best scores."
  (let ((scores (read-scores)))
    (pal:clear-screen (pal:color 0 0 0))
    (pal:draw-text "Cletris Scores" (pal:v 100 20))
    (loop for data in scores
       as x = 30
       as y = 50 then (+ y 55)
       do (pal:draw-text (format nil "~A ~A ~A"
                                 (first data) (second data) (third data))
                         (pal:v x y)))
    (pal::update-screen)
    (sleep 5)))


(defun left-right-handler (matrix block block-x block-y key)
  "Manage left or right BLOCK movement specified by KEY.
Return the new BLOCK-X."
  (let ((x block-x))
    (when (and block
               (or (equal :left key) (equal :right key)))
      (when *debug*
        (format t "~&~A ~A ~A " block block-x block-y))
      (let ((area-p (in-area-p matrix block block-x block-y key)))
        (when *debug*
          (format t "~&Block ~A ~A~%" key area-p))
        (when area-p
          (if (equal :left key)
              (setf x (1- block-x))
              (setf x (1+ block-x))))))
    x))


(defun finish-game (lines level points)
  "Finish the user's game."
  (when *debug*
    (format t "Game finished."))
  (pal:clear-screen (pal:color 0 0 0))
  (let ((r 65)
        (g 65)
        (b 65))
    (loop for y from 0 to 19
       as y0 = (+ (* y 20) +game-left-corner-y+)
       do (loop for x from 0 to 9
             as x0 = (+ (* x 20) +game-left-corner-x+)
             do (draw-square x0 y0 r g b))))
  (pal:draw-text (format nil "Level ~A" level) (pal:v 15 20))
  (pal:draw-text (format nil "Lines ~A" lines) (pal:v 15 65))
  (pal:draw-text (format nil "Points ~A" points) (pal:v 15 105))
  (pal:draw-text "'s' key to start a new game" (pal:v 15 250))
  (pal::update-screen)
  (sleep 3))


(defun get-points (nb)
  "Return the number of points from number of deleted lines."
  (cond ((= nb 1) 40)
        ((= nb 2) 100)
        ((= nb 3) 300)
        ((= nb 4) 1200)
        (t 0)))


(defclass game ()
  ((matrix :initform nil
           :initarg :matrix
           :accessor game-matrix)
   (block :initform nil
          :initarg :block
          :accessor game-block)
   (next-block :initform nil
               :initarg :next-block
               :accessor game-next-block)
   (block-x :initform nil
            :initarg :block-x
            :accessor game-block-x)
   (block-y :initform nil
            :initarg :block-y
            :accessor game-block-y)
   (level :initform 0
          :initarg :level
          :accessor game-level)
   (lines :initform 0
          :initarg :lines
          :accessor game-lines)
   (points :initform 0
           :initarg :points
           :accessor game-points))
  (:documentation "A new Tetris game."))


(defun make-game ()
  "Creates a new Tetris game client."
  (make-instance 'game
                 :matrix (init-matrix)
                 :block nil :next-block nil
                 :block-x nil :block-y nil
                 :level 0 :lines 0 :points 0))


(defgeneric delete-line (game number)
  (:documentation "Handler for when a a game-client delete a line."))

(defmethod delete-line ((game game) number)
  (format t "Delete line"))


(defun cletris (username &optional game)
  "Start a new Tetris game.
USERNAME could be used for the user name in a network session."
  (pal:with-pal (:width +width+ :height +height+
                 :fullscreenp nil :fps *speed*
                 :paths (concatenate 'string *cletris-directory* "font/"))
    (let ((rs (make-random-state t))
          (frame-count 0)
          (state :ready))
;;           networked)
;;     (if client
;;         (setf networked t)
;;         (setf networked nil
;;               client (make-client username nil nil nil nil (make-game))))
;;     (with-slots (matrix block next-block block-x block-y level lines points)
;;         (client-game client)
      (unless game
        (setf game (make-game)))
      (with-slots (matrix block next-block block-x block-y level lines points)
          game
        (pal:event-loop ()
          (case state
            (:ready
             (pal:test-keys
               (:key-a (display-about))
               (:key-v (display-scores))
               (:key-s (setf block (get-block rs)
                             next-block (get-block rs)
                             block-x 5
                             block-y 2
                             state :playing
                             matrix (init-matrix)))
               (:key-q (return-from pal:event-loop))))
            (:paused
             (pal:test-keys
               (:key-p (setf state :playing))))
            (:playing
             (pal:test-keys
               (:key-p (setf state :paused))
               (:key-left (setf block-x
                                (left-right-handler matrix block block-x block-y :left)))
               (:key-right (setf block-x
                                 (left-right-handler matrix block block-x block-y :right)))
               (:key-up (setf block (rotate matrix block block-x block-y)))
               (:key-down  (when (in-area-p matrix block block-x block-y :down)
                             (incf block-y)))
               (:key-space (loop
                              until (null
                                     (in-area-p matrix block block-x block-y :down))
                              do (incf block-y)
                                (setf frame-count 0)))
               (:key-h (when *debug*
                         (show-matrix matrix)))
               (:key-q (return-from pal:event-loop)))))
          (when (and (equal :playing state) block next-block)
            (incf frame-count)
            (when (> frame-count *speed*)
              (when *debug*
                (format t "~&Frame > speed ~A ~A" frame-count *speed*))
              (setf frame-count 0)
              (let ((area-p (in-area-p matrix block block-x block-y :down)))
                (when *debug*
                  (format t "~&Block ~A ~A ~A ~A ~% " block block-x block-y area-p))
                (if area-p
                    (incf block-y)
                    (progn
                      (update-matrix matrix block block-x block-y)
                      (let ((nb (clean-matrix matrix)))
                        (when (and (> nb 0)
                                   (>= (+ nb lines)
                                       (* 10 (1+ level))))
                          (decf *speed*)
                          (incf level))
                        ;; (when (> nb 1)
                        ;;   (delete-line game (1- nb)))
                        (if (<= block-y 2)
                            (progn
                              (finish-game lines level points)
                              (update-scores username points)
                              (setf matrix nil
                                    block nil
                                    next-block nil
                                    block-x nil
                                    block-y nil
                                    frame-count 0
                                    level 0
                                    lines 0
                                    state :ready))
                            (setf lines (+ lines nb)
                                  points (+ points (get-points nb))
                                  block next-block
                                  next-block (get-block rs)
                                  block-x 5
                                  block-y 2))))))))
          (when *debug*
            (format t "~&Frame ~A" frame-count))
          (draw-game matrix block next-block block-x block-y
                     lines level points username))))))
