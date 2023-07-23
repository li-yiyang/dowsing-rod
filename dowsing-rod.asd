;;; dowsing-rod.asd

(asdf:defsystem "dowsing-rod"
  :description ""
  :author "凉凉 <https://github.com/li-yiyang>"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("clog"                   ; GUI
               "dexador"                ; HTTP Client
               "yason"                  ; JSON Paraser
               "quri"                   ; URL Encoder
               )
  :serial T
  :components ((:file "src/js-convert")
               (:file "src/geojson")
               (:file "src/nominatim")
               (:file "src/clog-leaflet")
               (:file "dowsing-rod")
               (:file "dowsing-rod-user")))
