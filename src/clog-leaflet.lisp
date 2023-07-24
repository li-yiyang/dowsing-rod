(defpackage #:clog-leaflet
  (:use #:cl #:clog #:js-convert #:geojson)
  (:export #:create-map
           #:create-tile-layer
           #:create-geojson-layer)
  (:documentation "This is CLOG Wrapper for Leaflet."))

(in-package :clog-leaflet)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constant Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Leaflet Name Space

(defparameter *leaflet-namespace*
  "window.LeafletNameSpace"
  "Name Space to store the Leaflet objects. 
Leaflet objects can be referred by:
  <leaflet-namespace>['<class>-<id>'] 
in JS code.")

;;; Leaflet Variable

(defparameter *leaflet-css-path*
  "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
  "Path/URL to Leaflet CSS file.")

(defparameter *leaflet-js-path*
  "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
  "Path/URL to Leaflet Javascript file.")

(defparameter *default-map-center* '(51.505 -0.09)
  "Default `leaflet-map' center position.")

(defparameter *default-map-zoom* 13
  "Default `leaflet-map' zoom rate.")

(defparameter *default-zoom-ratio* 1
  "Default `zoom-ratio' for `zoom-in' and `zoom-out'.")

(defparameter *default-tile-layer-url*
  "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
  "Default Tile Layer URL.")

(defparameter *default-tile-layer-attribution*
  "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors"
  "Default Tile Layer Attribution.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOG Leaflet Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-leaflet (body)
  "Load the Leaflet Javascripts and opens Leaflet Namespace. 
It should be only called once or for earsing the Leaflet."
  (load-css    (html-document body)
               *leaflet-css-path*
               :load-only-once T)
  (load-script (html-document body)
               *leaflet-js-path*
               :load-only-once T
               :wait-for-load NIL)
  (js-execute body (format NIL "if (!(~A)) { ~A = {}; }"
                           *leaflet-namespace*
                           *leaflet-namespace*)))

;;;;;;;;;;;;;;;;;;;;
;;; CLOG Classes
;;;;;;;;;;;;;;;;;;;;

(defclass leaflet-obj ()
  ((leaflet-id   :initarg :leaflet-id
                 :initform (gensym "")
                 :reader leaflet-id))
  (:documentation "Basic Leaflet Object class."))

(defclass leaflet-map (clog-div leaflet-obj)
  ((layers :initform '()
           :type list
           :accessor layers)
   (markers :initform '()
            :type list
            :accessor markers))
  (:documentation "`leaflet-map' object bind to Leaflet Map object.
The `leaflet-map' object should act like a `clog-div' object.

The `layers' stores all the layers the `leaflet-map' use.
*Note that this is barely usable, nothing could be done with it.*

The `markers' stores all the markers the `leaflet-map' use."))

(defclass leaflet-layer (leaflet-obj) ()
  (:documentation "Leaflet Layer class binding."))

(defclass leaflet-marker (leaflet-layer)
  ((latitude :initform :latitude
             :accessor latitude)
   (longitude :initform :longitude
              :accessor longitude))
  (:documentation "`leaflet-marker' display icons on the map."))

(defclass leaflet-tile-layer (leaflet-layer)
  ((url :initarg :url
        :reader url)
   (attribution :initarg :attribution
                :reader attribution)
   (layers :initform '()
           :accessor layers))
  (:documentation "Used to load and display tile layers on the map."))

(defclass geojson-layer (leaflet-layer)
  ((geo-obj :initarg :geo-obj
            :accessor geo-obj))
  (:documentation "Add `geo-obj' as layer to the map."))

;;; Method and Functions

(defgeneric create-map (clog-obj &key center zoom width height
                                   style class html-id)
  (:documentation "Create a new `clog-leaflet' object on `clog-obj'.
The key parameters are described as below:
+ `center' the center position of `leaflet-map'. 
  It should be like `(longitude latitude)'.
+ `zoom' the zoom level of `leaflet-map'.
+ `width' and `height' is the size of `leaflet-map'.
  See more for `clog-div' properties.
+ `style', `class', `html-id' are same as `create-div'."))

(defmethod create-map ((obj clog-obj) &key (center *default-map-center*)
                                        (zoom *default-map-zoom*)
                                        (width "400px")
                                        (height "400px")
                                        (style NIL)
                                        (class NIL)
                                        (html-id NIL))
  (let ((map (create-div obj :html-id html-id
                             :class class
                             :style style)))
    (init-leaflet obj)
    (change-class map 'leaflet-map)
    (setf (width map) width
          (height map) height)
    (js-execute
     map
     (format NIL "~A = L.map('~A', ~A)"
             (->js map)
             (html-id map)
             (alist->js `((center . ,center)
                          (zoom   . ,zoom)))))))

(defgeneric create-tile-layer (map &key url attribution)
  (:documentation "Add Tile Layer to `map'. Return `leaflet-tile-layer' object."))

(defmethod create-tile-layer ((map leaflet-map) &key (url *default-tile-layer-url*)
                                               (attribution *default-tile-layer-attribution*))
  (let ((tile-layer (make-instance 'leaflet-tile-layer
                                   :url url
                                   :attribution attribution)))
    (js-execute map (format NIL "~A = L.tileLayer(~A, ~A).addTo(~A)"
                            (->js tile-layer)
                            (->js (url tile-layer))
                            (plist->js (list :attribution (attribution tile-layer)))
                            (->js map)))
    (push tile-layer (layers map))
    tile-layer))

(defgeneric create-geojson-layer (map geo-obj)
  (:documentation "Make a `geojson-layer' object and attach it to `map'."))

(defmethod create-geojson-layer ((map leaflet-map) geo-obj)
  (let ((geojson-layer (make-instance 'geojson-layer
                                      :geo-obj geo-obj)))
    (js-execute map
              (format NIL "~A = L.geoJSON(~A).addTo(~A)"
                      (->js geojson-layer)
                      (->js (geo-obj geojson-layer))
                      (->js map)))
    geojson-layer))

(defgeneric create-marker (map latitude longitude)
  (:documentation "Create `leaflet-marker' on `map'."))

(defmethod create-marker ((map leaflet-marker) latitude longitude)
  (let ((marker (make-instance 'leaflet-marker
                               :latitude latitude
                               :longitude longitude)))
    (js-execute map (format NIL "~A = L.marker([~A, ~A]).addTo(~A)"
                            (->js marker)
                            (latitude marker)
                            (longitude marker)
                            (->js map)))
    (push marker (markers map))
    marker))

(defmethod ->js ((obj leaflet-obj))
  (format NIL "~A['~A-~A']"
          *leaflet-namespace*
          (type-of obj)
          (leaflet-id obj)))

;;; JS Wrapper

(generate-js-wrapper
 leaflet-map
 (fly-to (lat-lng zoom &key (animate T) (duration 1))
         :doc "Sets the view of the map performing a smooth pan-zoom animation.")
 (set-view (lat-lng zoom &key (animate T)
                    (duration 0.5))
           :doc "Sets the view of the `leaflet-map' of `lat-lng' and `zoom' with options.
The `lat-lng' should be like `(latitude longitude)'. 
The options are described below:
+ `animate': If `T', panning will always be animated if possible. 
+ `duration': Duration of animated panning, in seconds.")
 (set-zoom (zoom)
           :doc "Sets the zoom of the map.")
 (zoom-in (&optional (zoom-ratio *default-zoom-ratio*))
          :doc "Increases the zoom of the map by `zoom-in'.")
 (zoom-out (&optional (zoom-ratio *default-zoom-ratio*))
           :doc "Decreases the zoom of the map by `zoom-ratio'."))

(generate-js-wrapper
 leaflet-layer
 (leaflet-remove ()
                 :doc "Remove leaflet object."
                 :js "remove")
 (bind-popup (message)
             :doc "Binds a popup to the layer with the passed `message'.")
 (unbind-popup ()
               :doc "Removes the popup previously bound with bindPopup.")
 (close-popup ()
              :doc "Closes the popup bound to this layer if it is open.")
 (open-popup ()
             :doc "Opens the bound popup.")
 (set-popup-content (message)
                    :doc "Sets `message' of the popup bound to this layer."))
