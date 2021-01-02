(in-package :trivial-types)

(defun setf-function-name-p (object)
  (and (listp object)
       (null (cddr object))
       (eq 'setf (car object))
       (symbolp (cadr object))))

(deftype function-name ()
  ;; Doesn't work great with subtypep
  "Ref: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name"
  `(or symbol (satisfies setf-function-name-p)))
