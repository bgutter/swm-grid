;;;; swm-grid.asd

(asdf:defsystem #:swm-grid
  :description "Grid-based Desktop Navigation in StumpWM"
  :author "Brandon Guttersohn <code@guttersohn.org>"
  :depends-on (#:stumpwm)
  :serial t
  :components ((:file "package")
               (:file "swm-grid")))
