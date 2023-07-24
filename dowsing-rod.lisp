;;; dowsing-rod.lisp

(defpackage #:dowsing-rod
  (:use #:cl #:clog #:clog-leaflet)
  (:export #:start-dowsing-rod)
  (:documentation "The dowsing rod is a map application."))

(in-package :dowsing-rod)

;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;

(defparameter *map* NIL
  "The `leaflet-map' binding to Leaflet Map object.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation - Start Dowsing Rod
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-dowsing-rod (&optional (target 'BROWSER))
  "Start Dowsing Rod Application.

The `target' should be:
+ `BROWSER' to open dowsing rod application in browser.

It will bind a special global variable `*map*' as `leaflet-map' object."
  (initialize (lambda (body)
                (set-margin body 0 0 0 0)
                (let* ((window (window body))
                       (width  (width  window))
                       (height (height window))
                       (map    (create-map body :width width
                                                :height height))
                       (tile-layer (create-tile-layer map)))
                  (declare (ignore tile-layer))
                  (set-on-resize window (lambda (win)
                                          (setf (width map) (width win)
                                                (height map) (height win))))
                  (setf *map* map))))
  (cond ((eq target 'BROWSER) (open-browser))))
