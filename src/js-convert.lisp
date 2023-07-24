;;; JS Convert Package
(defpackage #:js-convert
  (:use :cl)
  (:export #:wrapper
           #:merge-plist
           #:merge-alist
           #:->js
           #:plist->js
           #:alist->js
           #:generate-js-wrapper)
  (:documentation "Convert Common Lisp object to JavaScript Literally."))

(in-package :js-convert)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrapper (lst &key (left "[")
                      (right "]")
                      (spliter ", ")
                      (fn #'identity))
  "Wrap LST with LEFT and RIGHT, split element by SPLITER.
  The element will be mapped with FN function."
  (format NIL "~A~A~A"
          left
          (reduce (lambda (converted new)
                    (format NIL "~A~A~A"
                            converted spliter (funcall fn new)))
                  (rest lst)
                  :initial-value (funcall fn (first lst)))
          right))

(defun merge-plist (p1 p2)
  "Merge two property list `p1' and `p2'."
  (let ((res (copy-list p2)))
    (loop for (key value) on p1 by #'cddr
          if (not (getf res key))
            do (progn (push value res)
                      (push key res)))
    res))

(defun merge-alist (a1 a2)
  "Merge two association list `a1' and `a2'."
  (union a1 a2 :key #'car))

(defgeneric ->js (obj)
  (:documentation "Convert Common Lisp Object to JS code literally."))

(defmethod ->js (obj)
  "Default will be simply `format' function."
  (format NIL "~A" obj))

(defmethod ->js ((obj symbol))
  (cond ((eq obj T) "true")
        ((eq obj NIL) "false")
        (T (let ((char-list (coerce (string obj) 'list))
                 (upperp NIL))
             (wrapper char-list
                      :left "" :right "" :spliter ""
                      :fn (lambda (char)
                            (cond (upperp (setf upperp NIL)
                                          (string-upcase (string char)))
                                  ((eq char #\-) (setf upperp T) "")
                                  (T (string-downcase (string char))))))))))

(defmethod ->js ((obj number))
  (format NIL "~F" obj))

(defmethod ->js ((obj list))
  (wrapper obj :fn #'->js))

(defmethod ->js ((obj string))
  (format NIL "~S" obj))

(defmethod ->js ((obj hash-table))
  (wrapper (let ((res '()))
             (maphash
              (lambda (key value)
                (push (format NIL "~A: ~A"
                              (->js key)
                              (->js value))
                      res))
              obj)
             res)
           :left "{" :right "}"))

(defun alist->js (alist)
  "Turn `alist' into JS Map literally."
  (wrapper (mapcar (lambda (pair)
                     (format NIL "~A: ~A"
                             (->js (car pair))
                             (->js (cdr pair))))
                   alist)
           :left "{"
           :right "}"))

(defun plist->js (plist)
  "Turn `plist' into JS Map iterally."
  (if (null plist)
      "{}"
      (wrapper (loop for (key value) on plist by #'cddr
                     collect (format NIL "~A: ~A"
                                     (->js key) (->js value)))
               :left "{"
               :right "}")))

(defmacro generate-js-wrapper (class &rest definitions)
  "The `definitions' should be like:

   (method-name (parameters) options)

For example:
   (generate-js-wrapper example-class
     (example-method (parameters)
       :doc \"...\"))

For `parameters' there two special keywords: `&optional',
and `&key'. And the parameter name `key-options' is preseved. 

For `options' it shoule be like a property list:
+ `:doc' for documentaion
+ `:js' for special JavaScript method name,
  default method name should be `(->js method-name)'.
+ `:export' for whether export the function,
  default is `T'."
  (labels ((mk-para-lst (paras)
             (let ((para-lst '()))
               (loop for para in paras do
                 (cond ((eq para '&key)
                        (push '&rest para-lst)
                        (push 'key-options para-lst)
                        (push para para-lst))
                       (T (push para para-lst))))
               (reverse para-lst)))
           (flat-para-lst (paras)
             (loop for para in paras
                   while (not (eq para '&key))
                   if (not (eq para '&optional))
                     collect (if (listp para)
                                 (first para)
                                 para)))
           (generate (definition)
             (let* ((method    (first  definition))
                    (paras     (second definition))
                    (options   (cddr   definition))
                    (default-keys (let ((key-p NIL)
                                        (res '()))
                                    (loop for para in paras do
                                          (cond (key-p
                                                 (push (first para) res)
                                                 (push (second para) res))
                                                ((eq para '&key)
                                                 (setf key-p T))))
                                    (reverse res)))
                    (para-lst  (mk-para-lst paras))
                    (option-p  (find 'key-options para-lst))
                    (flat-para (flat-para-lst paras))
                    (->js-para (cons '(->js obj)
                                     (mapcar (lambda (para)
                                               `(->js ,para))
                                             flat-para)))
                    (js-name   (getf options :js (->js method)))
                    (export-p  (getf options :export T))
                    (docstr    (getf options :doc
                                     (format NIL "<~A>.~A(~A~A)"
                                             class
                                             js-name
                                             (wrapper flat-para
                                                      :left "" :right ""
                                                      :fn #'->js)
                                             (if option-p ", options" ""))))
                    (code
                      `((defgeneric ,method ,(cons 'obj
                                              (mapcar (lambda (para)
                                                        (if (listp para)
                                                            (first para)
                                                            para))
                                               para-lst))
                          (:documentation ,docstr))
                        (defmethod ,method ,(cons (list 'obj class) para-lst)
                          (declare ,(cons 'ignore
                                          (let ((res '())
                                                (key-p NIL))
                                            (loop for para in paras do
                                              (cond ((eq para '&key) (setf key-p T))
                                                    (key-p (push (if (listp para)
                                                                     (first para)
                                                                     para)
                                                                 res))))
                                            (reverse res))))
                          (clog:js-execute
                           obj
                           ,(append
                             `(format NIL
                                      ,(format NIL "~A.~A~A"
                                               "~A"
                                               js-name
                                               (wrapper (make-list (if option-p
                                                                       (1+ (length flat-para))
                                                                       (length flat-para))
                                                                   :initial-element "~A")
                                                        :left "(" :right ")")))
                             (if option-p
                                 (append ->js-para
                                         `((plist->js (merge-plist key-options
                                                                   (quote ,default-keys)))))
                                 ->js-para)))))))
               (if export-p
                   (append code `((export (quote ,method))))
                   code))))
    (cons 'progn (apply #'append (mapcar #'generate definitions)))))
