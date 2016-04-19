;;   Copyright 2016 Douglas Rocha
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; Operations Functors
(defun op-addition (x) (lambda (y) (+ y x)))
(defun op-subtraction (x) (lambda (y) (- y x)))
(defun op-multiplication (x) (lambda (y) (* y x)))
(defun op-division (x) (lambda (y) (/ y x)))

(defparameter op-list '(op-addition op-subtraction op-multiplication op-division))

(defun get-op (index value)
  (funcall (nth index op-list) value))

(defun get-random-value-and-op (max)
  (get-random-op (random-nonzero max)))

(defun get-random-op (value)
  (get-op (random (list-length op-list)) value))

(defun execute-op (operation initial-value)
  (funcall operation initial-value))

;; This function avoids generating division by zero errors
(defun random-nonzero (max)
  (1+ (random max)))

(defmacro compose (&rest args)
  (labels ((rec1 (args)
    (if (= (length args) 1)
      `(funcall ,@args x)
      `(funcall ,(first args) ,(rec1 (rest args))))))
    `(lambda (x) ,(rec1 args))))
