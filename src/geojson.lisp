;;; src/geojson.lisp

(defpackage #:geojson
  (:use :cl :js-convert)
  (:export #:->geo-obj
           #:->json
           #:geo-obj
           #:features))

(in-package :geojson)

;;;;;;;;;;;;;;;;;;;;;
;;; Class Binding
;;;;;;;;;;;;;;;;;;;;;

(defclass geojson-object ()
  ((geo-type :reader geo-type))
  (:documentation "The basic GeoJSON object."))

(defclass geojson-geometric-object (geojson-object)
  ((coordinates :initarg :coordinates
                :accessor coordinates))
  (:documentation "Basic geometric object without additional properties."))

(defclass point (geojson-geometric-object)
  ((geo-type :initform "Point"))
  (:documentation "Point `coordinates' are `(longitude latitude)'."))

(defclass line-string (geojson-geometric-object)
  ((geo-type :initform "LineString"))
  (:documentation "LineString `coordinates' are a list of `(longitude latitude)'."))

(defclass polygon (geojson-geometric-object)
  ((geo-type :initform "Polygon"))
  (:documentation "Polygon `coordinates' are list of linear ring coordinate arrays.
The first element in the list is the `exterior-ring'; the second element in the
list is the `interior-ring'."))

(defclass multi-point (geojson-geometric-object)
  ((geo-type :initform "MultiPolygon"))
  (:documentation "`multi-point' `coordinates' are list of `point' objects."))

(defclass multi-line-string (geojson-geometric-object)
  ((geo-type :initform "MultiLineString"))
  (:documentation "`multi-line-string' `coordinates' are list of `line-string' object."))

(defclass multi-polygon (geojson-geometric-object)
  ((geo-type :initform "MultiPolygon"))
  (:documentation "`multi-polygon' `coordinates' are list of `polygon' object."))

(defclass geometry-collection (geojson-geometric-object)
  ((geo-type :initform "GeometryCollection")
   (geometries :initarg :geometries
               :type geojson-geometric-object
               :accessor geometries))
  (:documentation "`geometries' are list of `geojson-geometric-object'."))

(defclass feature (geojson-object)
  ((geo-type :initform "Feature")
   (geometry :initarg :geometry
             :accessor geometry)
   (bbox :initarg :bbox
         :accessor bbox)
   (properties :initarg :properties
               :accessor properties)))

(defclass feature-collection (geojson-object)
  ((geo-type :initform "FeatureCollection")
   (bbox :initarg :bbox
         :accessor bbox)
   (features :initarg :features
             :accessor features))
  (:documentation "`feature-collection' is a collection of `feautre'."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table->geometric-object (dat)
  "Turn `dat' into `geojson-geometric-object'."
  (declare (type hash-table dat))
  (let ((type (gethash "type" dat)))
    (make-instance
     (cond ((string= type "Point")           'point)
           ((string= type "LineString")      'line-string)
           ((string= type "Polygon")         'polygon)
           ((string= type "MultiPoint")      'multi-point)
           ((string= type "MultiLineString") 'multi-line-string)
           ((string= type "MultiPolygon")    'multi-polygon))
     :coordinates (gethash "coordinates" dat))))

(defun hash-table->geometry-collection (dat) ;
  "Turn `dat' into `geometry-collection'."
  (declare (type hash-table dat))
  (make-instance 'geometry-collection
                 :geometries (mapcar #'hash-table->geometric-object
                                     (gethash "geometries" dat))))

(defun hash-table->feature (dat)
  "Turn `dat' into `feature'."
  (declare (type hash-table dat))
  (let ((bbox     (gethash "bbox" dat))
        (geometry (hash-table->geometric-object
                   (gethash "geometry" dat)))
        (properties (gethash "properties" dat)))
    (make-instance 'feature
                   :bbox bbox
                   :properties properties
                   :geometry geometry)))

(defun hash-table->feature-collection (dat)
  "Turn `dat' into `feature-collection'."
  (declare (type hash-table dat))
  (let ((bbox (gethash "bbox" dat))
        (features (mapcar #'hash-table->feature
                          (gethash "features" dat))))
    (make-instance 'feature-collection
                   :bbox bbox
                   :features features)))

(defun ->geo-obj (dat)
  (declare (type hash-table dat))
  (let ((type (gethash "type" dat)))
    (cond ((string= type "Feature")
           (hash-table->feature dat))
          ((string= type "FeatureCollection")
           (hash-table->feature-collection dat))
          ((string= type "GeometryCollection")
           (hash-table->geometry-collection dat)))))

(defun ->json (obj)
  "Turn `obj' into JSON form. Same as `->js'."
  (->js obj))

(defmethod ->js ((obj geojson-geometric-object))
  (format NIL "{\"type\": ~A, \"coordinates\": ~A}"
          (->js (geo-type obj))
          (->js (coordinates obj))))

(defmethod ->js ((obj geometry-collection))
  (format NIL "{\"type\": \"GeometryCollection\", \"geometries\": ~A}"
          (->js (geometries obj))))

(defmethod ->js ((obj feature))
  (format NIL "{\"type\": \"Feature\", \"geometry\": ~A, \"properties\": ~A}"
          (->js (geometry obj))
          (->js (properties obj))))

(defmethod ->js ((obj feature-collection))
  (format NIL "{\"type\": \"Feature\", \"features\": ~A}"
          (->js (features obj))))
