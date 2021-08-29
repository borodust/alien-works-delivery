(asdf:defsystem :alien-works-delivery-util
  :description "Utilities also embedded into delivery bundle"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :pathname "util/"
  :components ((:file "uti")))


(asdf:defsystem :alien-works-delivery
  :description "Utility to ship alien-works based applications"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:uiop :asdf :alexandria :cl-ppcre :split-sequence :alien-works-delivery-util
               :bodge-blobs-support)
  :license "MIT"
  :pathname "delivery/"
  :components ((:file "packages")
               (:file "registry")
               (:file "delivery")))


(asdf:defsystem :alien-works-delivery/appimage
  :description "Plugin for alien-works-delivery to deliver bundles in AppImage format"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:uiop :asdf :alexandria :alien-works-delivery)
  :license "MIT"
  :pathname "appimage/"
  :components ((:file "appimage")))
