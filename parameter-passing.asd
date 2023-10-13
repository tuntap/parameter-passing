(asdf:defsystem #:parameter-passing
  :description "Small DSL for exploring various parameter passing styles"
  :author "tun <tun@disroot.org>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "via-macros")
               (:file "via-macros-examples")
               (:file "via-functions")
               (:file "via-functions-examples"))
  :depends-on ("alexandria"
               "fare-quasiquote"
               "place-utils"
               "trivia"
               "trivia.quasiquote"))
