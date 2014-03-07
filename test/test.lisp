
;; Some tests 

(defparameter *s* (cletris::cletris-server "127.0.0.1" 9888))


(defparameter *c*
  (cletris::cletris-client "lam" "127.0.0.1" 9887 "127.0.0.1" 9888))

(defparameter *d*
  (cletris::cletris-client "nicolas" "127.0.0.1" 9886 "127.0.0.1" 9888))

(cletris:cletris "unittest")

