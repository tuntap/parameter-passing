(in-package #:parameter-passing.via-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion Examples

;;; Call by value

(defun func-by-value (a)
  ;; User code
  (setf a 10))

(defun example-by-value ()
  (let ((a 5))
    (func-by-value a)
    a))

;;; Call by reference

(defun func-by-reference-impl (abox)
  (symbol-macrolet ((a (peek abox)))
    ;; User code
    (setf a 10)
    (setf a 10)))

(defmacro func-by-reference (a)
  (check-place a)
  `(func-by-reference-impl (make-box ,a)))

(defun example-by-reference ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-reference (nth (incf i) a))
    ;; ERROR
    ;; (func-by-reference (+ i i))
    a))

;;; Call by constant

(defun func-by-constant (aconst)
  (symbol-macrolet ((a (constant-helper a aconst)))
    ;; User code
    ;; ERROR:
    ;; (setf a 10)
    a))

(defun example-by-constant ()
  (let ((a 5))
    (func-by-constant a)
    a))

;;; Call by result

(defun func-by-result-impl (abox)
  (let ((avar nil)
        (amarker nil))
    (unwind-protect
         (symbol-macrolet ((a (result-helper a avar amarker)))
           ;; User code
           ;; ERROR:
           ;; (print a)
           (setf a 10)
           (setf a 10))
      (when amarker
        (setf (peek abox) avar)))))

(defmacro func-by-result (a)
  (check-place a)
  `(func-by-result-impl (make-box ,a)))

(defun example-by-result ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-result (nth (incf i) a))
    ;; ERROR
    ;; (func-by-result (+ i i))
    a))

;;; Call by value-result

(defun func-by-value-result-impl (abox)
  (let ((avar (peek abox)))
    (unwind-protect
         (symbol-macrolet ((a avar))
           ;; User code
           (print a)
           (setf a 10)
           (setf a 10))
      (setf (peek abox) avar))))

(defmacro func-by-value-result (a)
  (check-place a)
  `(func-by-value-result-impl (make-box ,a)))

(defun example-by-value-result ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-value-result (nth (incf i) a))
    ;; ERROR
    ;; (func-by-value-result (+ i i))
    a))

;;; Call by name

(defun func-by-name-impl (abox)
  (symbol-macrolet ((a (name-helper a (peek abox))))
    ;; User code
    (setf a 10)
    (print a)))

(defmacro func-by-name (a)
  `(func-by-name-impl (make-evaluating-box ,a)))

(defun example-by-name ()
  (let ((a (list 5 5 5))
        (i 0))
    (func-by-name (nth (incf i) a))
    ;; ERROR
    ;; (func-by-name (+ 1 1))
    a))
