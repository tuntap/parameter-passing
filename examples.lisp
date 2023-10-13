(in-package #:parameter-passing.via-functions)

(declaim (optimize (debug 3) (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Examples

;;; TODO: Jensen's device is pretty weird because the index parameter that's
;;; passed by name is also replaced within the part of the for loop that
;;; introduces the name of the counter variable. This is normally not a that's
;;; "evaluated" and is instead just part of the syntax of the for loop.
;;;
;;; Is ALGOL's substitution (copy) rule really defined completely "textually",
;;; without differentiating whether something is an expression to be evaluated
;;; vs. a "name" ("part of the syntax")?
;;;
;;; In our example we have to manually introduce the counter variable outside of
;;; the SUM function and explicitly pass it in by name as the first argument.
;;; This is something that the original Jensen's device doesn't have to do.

;;; Jensen's device

(defunpp sum ((:name var) begin end (:name expr))
  (let ((s 0))
    (loop :initially (setf var begin)
          :until (= var end)
          :do (incf s expr)
              (incf var)
          :finally (return s))))

(defun example-sum ()
  (let (i)
    (sum i 1 101 i)))

;;; => 5050

;;; Exercise 4.6

(defun ex-4-6 ()
  (let ((x 2))
    (labelspp ((fie ((:val y))
                    (incf x y)))
      (let ((x 5))
        (fie x)
        (print x)))
    (print x)))

;;; >> 5 7

;;; Exercise 4.7

(defun ex-4-7 ()
  (let ((x 2))
    (declare (special x))
    (labelspp ((fie ((:ref y))
                    (incf x y)))
      (let ((x 5))
        (declare (special x))
        (fie x)
        (print x)))
    (print x)))

;;; >> 10 2

;;; Exercise 4.8

(defun ex-4-8 ()
  (let ((x 2))
    (labelspp ((fie ((:ref y))
                    (incf x y)
                    (incf y)))
      (let ((x 5)
            (y 5))
        (declare (ignore y))
        (fie x)
        (print x)))
    (print x)))

;;; >> 6 7

;;; Exercise 4.9

(defun ex-4-9 ()
  (let ((x 2))
    (labelspp ((fie ((:val y))
                    (incf x y)))
      (let ((x 5))
        (fie (prog1 x (incf x)))
        (print x)))
    (print x)))

;;; >> 6 7

;;; Exercise 4.10

(defun ex-4-10 ()
  (let ((x 2))
    (labelspp ((fie ((:name y))
                    (incf x y)))
      (let ((x 5))
        (let ((x 7))
          (declare (ignore x)))
        (fie (prog1 x (incf x)))
        (print x)))
    (print x)))

;;; >> 6 7

;;; Exercise 4.11

(defun ex-4-11 ()
  (let ((x 1)
        (y 1))
    (declare (special x y))
    (labelspp ((fie ((:ref z))
                    (incf z (+ x y))))
      (let ((y 3))
        (declare (special y))
        (let ((x 3))
          (declare (special x)))
        (fie y)
        (print y)))
    (print y)))


;;; >> 7 1

;;; Exercise 4.12

(defun ex-4-12 ()
  (let ((x 0))
    (labelspp ((a ((:ref y))
                  (let ((x 2))
                    (incf y)
                    (+ (b y) x)))
               (b ((:ref y))
                  (labelspp ((c ((:ref y))
                                (let ((x 3))
                                  (+ (a y) x y))))
                    (if (= y 1)
                        (+ (c x) y)
                        (+ x y)))))
      (print (a x)))))

;;; >> 15

;;; Exercise 4.13

(defun ex-4-13-1 ()
  (let ((z 0))
    (labelspp ((omega ()
                      (omega))
               (foo ((:val x) (:val y))
                    (if (= x 0)
                        x
                        (+ x y))))
      (print (foo z (+ (omega) z))))))

;;; Infinite loop

(defun ex-4-13-2 ()
  (let ((z 0))
    (labelspp ((omega ()
                      (omega))
               (foo ((:name x) (:name y))
                    (if (= x 0)
                        x
                        (+ x y))))
      (print (foo z (+ (omega) z))))))

;;; >> 0

;;; Exercise 6.7

(defun ex-6-7 ()
  (let ((x 5))
    (labelspp ((p ((:name m))
                  (let ((x 2))
                    (+ m x))))
      (print (+ (p (prog1 x (incf x))) x)))))

;;; >> 13

;;; Exercise 7.2

(defun ex-7-2 ()
  (let ((x (make-array 10))
        (i 1))
    (setf (aref x 0) 0
          (aref x 1) 0
          (aref x 2) 0)
    (labelspp ((foo ((:ref y) (:ref j))
                    (setf (aref x j) (+ j 1))
                    (print y)
                    (incf j)
                    (setf (aref x j) j)
                    (print y)))
      (foo (aref x i) i))
    (print (aref x i))))

;;; >> 2 2 2

;;; Exercise 7.3

(defun ex-7-3 ()
  (let ((x 2))
    (labelspp ((foo ((:valres y))
                    (incf y)
                    (print x)
                    (incf y)))
      (foo x))
    (print x)))

;;; >> 2 4

;;; Exercise 7.5

(defun ex-7-5 ()
  (let ((x 2))
    (labelspp ((foo ((:name y))
                    (incf x)
                    (print y)
                    (incf x)))
      (foo (+ x 1)))
    (print x)))

;;; >> 4 4
