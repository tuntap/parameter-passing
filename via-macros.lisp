(defpackage #:parameter-passing.via-macros
  (:use #:cl))

(in-package #:parameter-passing.via-macros)

(named-readtables:in-readtable :fare-quasiquote)

;;; DONE: It turns out that it's not possible to implement parameter passing
;;; styles using *just* macros like it has been done here. Because the defined
;;; functions are really macros behind the scenes, calling a function becomes
;;; the expansion of the macro's body. Since the macros contain user-defined
;;; code, variable capture is possible when there are e.g. multiple nested
;;; scopes, with each one introducing a binding for a variable of the same name.

;;; NOTE: We can get rid of unused function warnings by pretending to use the
;;; helper functions within a lambda that's bound to a dummy variable but never
;;; called. This gets rid of the warnings while avoiding potential side effects.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support

(defun placep (form &optional env)
  (trivia:match (handler-case (nth-value 3 (get-setf-expansion form env))
                  (condition nil))
    (nil nil)
    (`(funcall #'(setf ,name) ,@_) (fboundp `(setf ,name)))
    (_ t)))

(defun check-place (form)
  (unless (placep form)
    (error "~s is not a place" form)))

(define-setf-expander constant-helper (param form)
  (declare (ignore form))
  (error "Cannot assign to constant parameter ~s" param))

(defmacro constant-helper (param form)
  (declare (ignore param))
  form)

(define-setf-expander result-helper (param var marker &environment env)
  (declare (ignore param))
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion var env)
    (values
     temps
     values
     stores
     `(progn ,set (setf ,marker t))
     get)))

(defmacro result-helper (param var marker)
  (declare (ignore var marker))
  (error "Cannot read the result parameter ~s" param))

(define-setf-expander name-helper (original-place place &environment env)
  (check-place original-place)
  (get-setf-expansion place env))

(defmacro name-helper (original-form form)
  (declare (ignore original-form))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation

(named-readtables:in-readtable :standard)

(defun make-generator-for-value (param)
  (values
   (lambda (form)
     `(alexandria:once-only (,param)
        `,,form))
   ``((,',param ,,param))))

(defun make-generator-for-reference (param)
  (alexandria:with-gensyms (gresolved gfun gvalue gtmp1 gtmp2)
    (values
     (lambda (form)
       `(progn
          (check-place ,param)
          (alexandria:with-gensyms (,gresolved ,gfun ,gvalue ,gtmp1 ,gtmp2)
            `(place-utils:with-resolved-places ((,,gresolved ,,param))
               (flet ((,,gfun ()
                        ,,gresolved)
                      ((setf ,,gfun) (,,gvalue)
                        (setf ,,gresolved ,,gvalue)))
                 ;; A trick to avoid "unused function" warnings.
                 (let ((,,gtmp1 (lambda () (funcall #',,gfun)))
                       (,,gtmp2 (lambda () (funcall #'(setf ,,gfun) nil))))
                   (declare (ignore ,,gtmp1 ,,gtmp2))
                   ,,form))))))
     ``((,',param (,,gfun))))))

(defun make-generator-for-constant (param)
  (values
   (lambda (form)
     `(alexandria:once-only (,param)
        `,,form))
   ``((,',param (constant-helper ,',param ,,param)))))

(defun make-generator-for-result (param)
  (alexandria:with-gensyms (gvar gmarker)
    (values
     (lambda (form)
       `(progn
          (check-place ,param)
          (alexandria:with-gensyms (,gvar ,gmarker)
            `(let ((,,gmarker nil)
                   (,,gvar nil))
               (unwind-protect ,,form
                 (when ,,gmarker
                   (setf ,,param ,,gvar)))))))
     ``((,',param (result-helper ,',param ,,gvar ,,gmarker))))))

(defun make-generator-for-value-result (param)
  (alexandria:with-gensyms (gvar gresolved gfun gvalue)
    (values
     (lambda (form)
       `(progn
          (check-place ,param)
          (alexandria:with-gensyms (,gvar ,gresolved ,gfun ,gvalue)
            `(place-utils:with-resolved-places ((,,gresolved ,,param))
               (flet ((,,gfun ()
                        ,,gresolved)
                      ((setf ,,gfun) (,,gvalue)
                        (setf ,,gresolved ,,gvalue)))
                 (let ((,,gvar (,,gfun)))
                   (unwind-protect ,,form
                     (setf (,,gfun) ,,gvar))))))))
     ``((,',param ,,gvar)))))

(defun make-generator-for-name (param)
  (alexandria:with-gensyms (gfun gvalue gtmp1 gtmp2)
    (values
     (lambda (form)
       `(alexandria:with-gensyms (,gfun ,gvalue ,gtmp1 ,gtmp2)
          `(flet ((,,gfun ()
                    ,,param)
                  ,@(when (placep ,param)
                      `(((setf ,,gfun) (,,gvalue)
                         (setf ,,param ,,gvalue)))))
             ;; A trick to avoid "unused function" warnings.
             (let ((,,gtmp1 (lambda () (funcall #',,gfun)))
                   ,@(when (placep ,param)
                       `((,,gtmp2 (lambda () (funcall #'(setf ,,gfun) nil))))))
               (declare (ignore ,,gtmp1
                                ,@(when (placep ,param)
                                    `(,,gtmp2)))))
             ,,form)))
     ``((,',param (name-helper ,,param (,,gfun)))))))

(defun generate-body (specs body)
  (let* ((smacrolets (mapcar #'second specs)))
    (reduce
     (lambda (spec acc)
       (destructuring-bind (generator smacrolet param) spec
         (declare (ignore smacrolet param))
         (funcall generator acc)))
     specs
     :initial-value ``(symbol-macrolet ,(append ,@smacrolets)
                        ,@',body)
     :from-end t)))

(defun normalize-param-spec (spec)
  (destructuring-bind (mode param)
      (etypecase spec
        (symbol `(:val ,spec))
        (list spec))
    (append (multiple-value-list
             (funcall
              (ecase mode
                (:val #'make-generator-for-value)
                (:ref #'make-generator-for-reference)
                (:const #'make-generator-for-constant)
                (:res #'make-generator-for-result)
                (:valres #'make-generator-for-value-result)
                (:name #'make-generator-for-name))
              param))
            (list param))))

(defmacro defunpp (name specs &body body)
  (let* ((specs (mapcar #'normalize-param-spec specs))
         (params (mapcar #'third specs)))
    `(defmacro ,name ,params
       ,(generate-body specs body))))

(defmacro labelspp (labels &body body)
  `(macrolet ,(loop :for (name specs . body) :in labels
                    :for nspecs := (mapcar #'normalize-param-spec specs)
                    :for params := (mapcar #'third nspecs)
                    :collect `(,name ,params ,(generate-body nspecs body)))
     ,@body))
