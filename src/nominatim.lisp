(defpackage #:nominatim
  (:use :cl :js-convert)
  (:export #:search-query))

(in-package :nominatim)

;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;

(defparameter *nominatim-host*
  "https://nominatim.openstreetmap.org"
  "The base URL of Nominatim API.")

(defparameter *nominatim-proxy* NIL
  "Proxy URL to use Nominatim API. Set `NIL' for no proxy.")

(defparameter *nominatim-fetch-parameters*
  '(("format" . "geojson"))
  "Deafult fetch parameters.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fetch (method parameters &key (proxy *nominatim-proxy*))
  "The general fetch method for wrap API usage."
  (let* ((url (quri:render-uri
               (quri:make-uri
                :defaults *nominatim-host*
                :path method
                :query (merge-alist parameters
                                    *nominatim-fetch-parameters*))))
         (res (if proxy
                  (dex:get url :proxy proxy)
                  (Dex:Get url))))
    (values (yason:parse res) res)))

(defun search-query (query &optional parameters (proxy *nominatim-proxy*))
  "Free form `query' string to search for. 

    Free-form queries are processed first left-to-right and then right-to-left 
    if that fails. So you may search for pilkington avenue, birmingham as well 
    as for birmingham, pilkington avenue. Commas are optional, but improve 
    performance by reducing the complexity of the search."
  (fetch "search"
         (merge-alist `(("q" . ,query))
                      parameters)
         :proxy proxy))

(defun search-amenity (amenity &optional parameters (proxy *nominatim-proxy*))
  "`amenity' can be the name or type of POI."
  (fetch "search"
         (merge-alist `(("amenity" . ,amenity))
                      parameters)
         :proxy proxy))

(defun reverse-search (latitude longtitude &optional parameters
                                             (proxy *nominatim-proxy*))
  "Reverse geocoding generates an address from a latitude and longitude."
  (fetch "reverse"
         (merge-alist `(("lat" . ,latitude)
                        ("lon" . ,longtitude))
                      parameters)
         :proxy proxy))
