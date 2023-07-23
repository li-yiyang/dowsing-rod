;;; dowsing-rod-user.lisp

(defpackage #:dowsing-rod-user
  (:use #:dowsing-rod #:clog)
  (:export #:start-dowsing-rod)
  (:documentation "Feel free to use the Dowsing Rod."))

;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;

(defparameter *map* NIL
  "The `leaflet-map' binding to Leaflet Map object.")

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
                  (set-on-resize window (lambda (win)
                                          (setf (width map) (width win)
                                                (height map) (height win))))
                  (setf *map* map))))
  (cond ((eq target 'BROWSER) (open-browser))))
