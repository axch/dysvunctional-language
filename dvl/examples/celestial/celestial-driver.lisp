;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(defmacro delay (exp)
  `(lambda () ,exp))

(defun force (thunk)
  (funcall thunk))

(defun constant-arg-for-dvl-stream (argument dvl-stream)
  (labels ((do-it (dvl-stream)
             (cons (car dvl-stream)
                   (delay (do-it (funcall (cdr dvl-stream) argument))))))
    (do-it dvl-stream)))

(defun stream-for-each (f stream)
  (cons (funcall f (car stream))
        (delay (stream-for-each f (force (cdr stream))))))

(defun stream-take (count stream)
  (if (= count 0)
      stream
      (stream-take (- count 1) (force (cdr stream)))))

(defun drive (count step dt)
  (stream-take count
   (constant-arg-for-dvl-stream (coerce dt 'double-float)
    (funcall (__main__) (coerce step 'double-float)))))
