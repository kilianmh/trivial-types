(in-package :trivial-types)
(5am:in-suite :trivial-types)

(declaim (inline proper-list-p
                 property-list-p
                 association-list-p
                 tuplep))

(defmacro %proper-list-p (var &optional (element-type '*))
  ;; ELEMENT-TYPE is not used by PROPER-LIST-P; however, it is used by some
  ;; other constructs in this file
  `(loop
     (typecase ,var
       (null (return t))
       (cons (if (or ,(eq element-type '*)
                     (typep (car ,var) ,element-type))
                 (setq ,var (cdr ,var))
                 (return)))
       (t    (return)))))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list.

Examples:

    (proper-list-p 1) => NIL
    (proper-list-p '(1 . 2)) => NIL
    (proper-list-p nil) => T
    (proper-list-p '(1 2 3)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

(defpackage trivial-types/proper-list (:use))

(deftype proper-list (&optional (element-type '*))
  "Equivalent to `(or null (and list (satisfies proper-list-p)))`.

Examples:

    (typep '(1 2 3) '(proper-list integer)) => T
    (typep '(1 2 3) '(proper-list string)) => NIL"
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/04_bc.htm
  (setq element-type
        (etypecase element-type
          ((or standard-class structure-class) (class-name element-type))
          ((or symbol list)                    element-type)))
  `(or null
       (and list (satisfies proper-list-p)
            ,(if (eq element-type '*)
                 t
                 (let* ((predicate-name   (format nil "LIST OF ~S" element-type))
                        (predicate-symbol (intern predicate-name :trivial-types/proper-list))
                        (list             (gensym))
                        (elt              (gensym)))
                   (compile predicate-symbol
                            `(lambda (,list)
                               (loop :for ,elt :in ,list :always (typep ,elt ',element-type))))
                   `(satisfies ,predicate-symbol))))))

(5am:def-test proper-list ()
  (5am:is-true  (typep '(1 2 3) 'proper-list))
  #+(or sbcl ccl)
  (5am:is-true  (typep (list (make-array 1 :element-type 'single-float :initial-element 0.0f0))
                       '(proper-list (array single-float))))
  (5am:is-true  (typep '(1 2 3) '(proper-list integer)))
  (5am:is-false (typep '(1 2 3) '(proper-list string)))
  (5am:is-false (typep '(1 . 2) 'proper-list))
  (5am:is-true  (subtypep 'null 'proper-list)))

(defun property-list-p (object)
  "Returns true if OBJECT is a property list.

Examples:

    (property-list-p 1) => NIL
    (property-list-p '(1 2 3)) => NIL
    (property-list-p '(foo)) => NIL
    (property-list-p nil) => T
    (property-list-p '(foo 1)) => T
    (property-list-p '(:a 1 :b 2)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (typecase object
    (null t)
    (cons
     (loop
       (if (null object)
           (return t)
           (let ((key (car object))
                 (next (cdr object)))
             (if (or (not (symbolp key))
                     (not (consp next)))
                 (return)
                 (setq object (cdr next)))))))))

(deftype property-list (&optional (value-type '*))
  "Equivalent to `(and list (satisfies
property-list-p))`. VALUE-TYPE is just ignored.

Examples:

    (typep '(:a 1 :b 2) '(property-list integer)) => T
    (typep '(:a 1 :b 2) '(property-list string)) => T"
  (declare (ignore value-type))
  '(and list (satisfies property-list-p)))

(defun association-list-p (var)
  "Returns true if OBJECT is an association list.

Examples:

    (association-list-p 1) => NIL
    (association-list-p '(1 2 3)) => NIL
    (association-list-p nil) => T
    (association-list-p '((foo))) => T
    (association-list-p '((:a . 1) (:b . 2))) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p var 'cons))

(deftype association-list (&optional (key-type '*) (value-type '*))
  "Equivalent to `(proper-list (cons KEY-TYPE VALUE-TYPE))`. KEY-TYPE
and VALUE-TYPE are just ignored.

Examples:

    (typep '((:a . 1) (:b . 2)) '(association-list integer)) => T
    (typep '((:a . 1) (:b . 2)) '(association-list string)) => T"
  `(proper-list (cons ,key-type ,value-type)))

(defun tuple (&rest args)
  "Exactly same as LIST."
  (declare (optimize . #.*standard-optimize-qualities*))
  args)

(defun tuplep (object)
  "Returns true if OBJECT is a tuple, meaning a proper list.

Examples:

    (tuplep 1) => NIL
    (tuplep '(1 . 2)) => NIL
    (tuplep nil) => T
    (tuplep '(1 2 3)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

(deftype tuple (&rest element-types)
  "Equivalent to `(and list (cons ARG1 (cons ARG2 (cons ARG3 ...))))`
where ARGn is each element of ELEMENT-TYPES.

Examples:

    (typep 1 'tuple) => NIL
    (typep '(1 . 2) 'tuple) => NIL
    (typep '(1 2 3) 'tuple) => NIL
    (typep '(1 2 3) '(tuple integer integer)) => NIL
    (typep '(1 2 3) '(tuple string integer integer)) => NIL
    (typep nil 'tuple) => T
    (typep nil '(tuple)) => T
    (typep '(1 2 3) '(tuple integer integer integer)) => T"
  `(and list
        ,(reduce (lambda (element-type type) `(cons ,element-type ,type))
                 element-types
                 :from-end t
                 :initial-value 'null)))
