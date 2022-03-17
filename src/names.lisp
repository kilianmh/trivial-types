(in-package :trivial-types)

(deftype function-name ()
  "Ref: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_name"
  `(or symbol
       (cons (eql cl:setf)
             (cons symbol null))))
