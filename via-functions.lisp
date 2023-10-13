(defpackage #:parameter-passing.via-functions
  (:use #:cl))

(in-package #:parameter-passing.via-functions)

(named-readtables:in-readtable :fare-quasiquote)

;;; TODO: Should the defined functions be named by a gensym or have their names
;;; generated in a particular way? If not, do we have to be worried about
;;; producing garbage? If yes, should this be customizable so that the user can
;;; decide what the name is?

;;; TODO: Handle declarations and documentation strings.

;;; TODO: What about dynamic scoping? Does it even make sense for reference,
;;; result, value-result and name parameters to be able to have dynamic scope?
;;; Value and constant parameters should be able to support dynamic scoping just
;;; fine.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support

;;; NOTE: PLACEP has a couple of caveats. Since it relies on the presence of
;;; SETF functions, SETF functions that are defined *after* the macros that make
;;; use of PLACEP will end up not being detected. Furthermore, even if they were
;;; defined *before*, they still wouldn't be detected within the same
;;; compilation unit unless they were wrapped in an appropriate EVAL-WHEN.

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

(defmacro make-box (form &key setp)
  (alexandria:with-gensyms (gcommand gvalue)
    `(lambda (,gcommand &optional ,gvalue)
       ,@(unless setp `((declare (ignore ,gvalue))))
       (ecase ,gcommand
         (:get ,form)
         ,@(when setp `((:set (setf ,form ,gvalue))))))))

(defmacro make-once-only-box (form &key setp)
  (if setp
      (alexandria:with-gensyms (gresolved)
        `(place-utils:with-resolved-places ((,gresolved ,form))
           (make-box ,gresolved :setp t)))
      (alexandria:once-only (form)
        `(make-box ,form))))

(defun peek (box)
  (funcall box :get))

(defun (setf peek) (value box)
  (funcall box :set value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation

(defun make-generator-for-value (param)
  (values #'identity param '()))

(defun make-generator-for-reference (param)
  (alexandria:with-gensyms (gbox)
    (values #'identity gbox `((,param (peek ,gbox))))))

(defun make-generator-for-constant (param)
  (alexandria:with-gensyms (gconst)
    (values #'identity gconst `((,param (constant-helper ,param ,gconst))))))

(defun make-generator-for-result (param)
  (alexandria:with-gensyms (gbox gvar gmarker)
    (values
     (lambda (form)
       `(let ((,gvar nil)
              (,gmarker nil))
          (unwind-protect ,form
            (when ,gmarker
              (setf (peek ,gbox) ,gvar)))))
     gbox
     `((,param (result-helper ,param ,gvar ,gmarker))))))

(defun make-generator-for-value-result (param)
  (alexandria:with-gensyms (gbox gvar)
    (values
     (lambda (form)
       `(let ((,gvar (peek ,gbox)))
          (unwind-protect ,form
            (setf (peek ,gbox) ,gvar))))
     gbox
     `((,param ,gvar)))))

(defun make-generator-for-name (param)
  (alexandria:with-gensyms (gbox)
    (values #'identity gbox `((,param (name-helper ,param (peek ,gbox)))))))

(defun generate-function-body (specs body)
  (let* ((smacrolets (mapcar #'fifth specs)))
    (reduce
     (lambda (spec acc)
       (destructuring-bind (mode param generator impl-param smacrolet) spec
         (declare (ignore mode param impl-param smacrolet))
         (funcall generator acc)))
     specs
     :initial-value `(symbol-macrolet ,(apply #'append smacrolets)
                       ,@body)
     :from-end t)))

(defun generate-macro-body (fun specs)
  (multiple-value-bind (checks args)
      (loop :for spec :in specs
            :for (mode param generator impl-param smacrolet) := spec
            :when (member mode '(:ref :res :valres))
              :collect `(check-place ,param) :into checks
            :collect (ecase mode
                       ((:val :const)
                        param)
                       ((:ref :res :valres)
                        ``(make-once-only-box ,,param :setp ,(placep ,param)))
                       (:name
                        ``(make-box ,,param :setp ,(placep ,param))))
              :into args
            :finally (return (values checks args)))
    `(progn
       ,@checks
       `(,',fun ,,@args))))

(defun normalize-param-spec (spec)
  (destructuring-bind (mode param)
      (etypecase spec
        (symbol `(:val ,spec))
        (list spec))
    (append (list mode param)
            (multiple-value-list
             (funcall
              (ecase mode
                (:val #'make-generator-for-value)
                (:ref #'make-generator-for-reference)
                (:const #'make-generator-for-constant)
                (:res #'make-generator-for-result)
                (:valres #'make-generator-for-value-result)
                (:name #'make-generator-for-name))
              param)))))

(defmacro defunpp (name specs &body body)
  (let* ((specs (mapcar #'normalize-param-spec specs))
         (params (mapcar #'second specs))
         (impl-params (mapcar #'fourth specs)))
    (alexandria:with-gensyms (gfun)
      `(progn
         (defun ,gfun ,impl-params
           ,(generate-function-body specs body))
         (defmacro ,name ,params
           ,(generate-macro-body gfun specs))))))

(defmacro labelspp (labels &body body)
  (multiple-value-bind (funs macrolets)
      (loop :for (name specs . body) :in labels
            :for nspecs := (mapcar #'normalize-param-spec specs)
            :for params := (mapcar #'second nspecs)
            :for impl-params := (mapcar #'fourth nspecs)
            :for gfun := (gensym (string 'gfun))
            :collect `(,gfun ,impl-params ,(generate-function-body nspecs body))
              :into funs
            :collect `(,name ,params ,(generate-macro-body gfun nspecs))
              :into macrolets
            :finally (return (values funs macrolets)))
    `(macrolet ,macrolets
       (labels ,funs
         ,@body))))
