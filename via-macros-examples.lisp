(in-package #:parameter-passing.via-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion Examples

;;; Call by value

(defmacro func-by-value (a)
  (alexandria:once-only (a)
    `(symbol-macrolet ((a ,a))
       ;; User code
       (setf a 10))))

(defun example-by-value ()
  (let ((a 5))
    (func-by-value a)
    a))

;;; Call by reference

(defmacro func-by-reference (a)
  (check-place a)
  (alexandria:with-gensyms (gresolved gfun gvalue gtmp1 gtmp2)
    `(place-utils:with-resolved-places ((,gresolved ,a))
       (flet ((,gfun ()
                ,gresolved)
              ((setf ,gfun) (,gvalue)
                (setf ,gresolved ,gvalue)))
         ;; A trick to avoid "unused function" warnings.
         (let ((,gtmp1 (lambda () (funcall #',gfun)))
               (,gtmp2 (lambda () (funcall #'(setf ,gfun) nil))))
           (declare (ignore ,gtmp1 ,gtmp2))
           (symbol-macrolet ((a (,gfun)))
             ,@'(;; User code
                 (setf a 10)
                 (setf a 10))))))))

(defun example-by-reference ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-reference (nth (incf i) a))
    ;; ERROR
    ;; (func-by-reference (+ i i))
    a))

;;; Call by constant

(defmacro func-by-constant (a)
  (alexandria:once-only (a)
    `(symbol-macrolet ((a (constant-helper a ,a)))
       ;; User code
       ;; ERROR:
       ;; (setf a 10)
       a)))

(defun example-by-constant ()
  (let ((a 5))
    (func-by-constant a)
    a))

;;; Call by result

(defmacro func-by-result (a)
  (check-place a)
  (alexandria:with-gensyms (gvar gmarker)
    `(let ((,gmarker nil)
           (,gvar nil))
       (unwind-protect
            (symbol-macrolet ((a (result-helper a ,gvar ,gmarker)))
              ,@'(;; User code
                  ;; ERROR:
                  ;; (print a)
                  (setf a 10)
                  (setf a 10)))
         (when ,gmarker
           (setf ,a ,gvar))))))

(defun example-by-result ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-result (nth (incf i) a))
    ;; ERROR
    ;; (func-by-result (+ i i))
    a))

;;; Call by value-result

(defmacro func-by-value-result (a)
  (check-place a)
  (alexandria:with-gensyms (gresolved gvar gfun gvalue)
    `(place-utils:with-resolved-places ((,gresolved ,a))
       (flet ((,gfun ()
                ,gresolved)
              ((setf ,gfun) (,gvalue)
                (setf ,gresolved ,gvalue)))
         (let ((,gvar (,gfun)))
           (unwind-protect
                (symbol-macrolet ((a ,gvar))
                  ,@'(;; User code
                      (print a)
                      (setf a 10)
                      (setf a 10)))
             (setf (,gfun) ,gvar)))))))

(defun example-by-value-result ()
  (let ((a (list 5 5))
        (i 0))
    (func-by-value-result (nth (incf i) a))
    ;; ERROR
    ;; (func-by-value-result (+ i i))
    a))

;;; Call by name

(defmacro func-by-name (a)
  (alexandria:with-gensyms (gfun gvalue gtmp1 gtmp2)
    `(flet ((,gfun ()
              ,a)
            ,@(when (placep a)
                `(((setf ,gfun) (,gvalue)
                   (setf ,a ,gvalue)))))
       (let ((,gtmp1 (lambda () (funcall #',gfun)))
             ,@(when (placep a)
                 `((,gtmp2 (lambda () (funcall #'(setf ,gfun) nil))))))
         (declare (ignore ,gtmp1
                          ,@(when (placep a)
                              `(,gtmp2))))
         (symbol-macrolet ((a (name-helper ,a (,gfun))))
           ;; User code
           (setf a 10)
           (print a))))))

(defun example-by-name ()
  (let ((a (list 5 5 5))
        (i 0))
    (func-by-name (nth (incf i) a))
    ;; ERROR
    ;; (func-by-name (+ 1 1))
    a))

