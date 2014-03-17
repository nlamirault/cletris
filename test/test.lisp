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

(defparameter *s* (cletris::cletris-server "127.0.0.1" 9888))


(defparameter *c*
  (cletris::cletris-client "lam" "127.0.0.1" 9887 "127.0.0.1" 9888))

(defparameter *d*
  (cletris::cletris-client "nicolas" "127.0.0.1" 9886 "127.0.0.1" 9888))

(cletris:cletris "unittest")
