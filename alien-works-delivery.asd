(asdf:defsystem :alien-works-delivery/util
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
  :depends-on (:uiop :asdf :alexandria :cl-ppcre :alien-works-delivery/util
               :bodge-blobs-support :distignore :trivial-features)
  :license "MIT"
  :pathname "delivery/"
  :components ((:file "packages")
               (:file "registry")
               (:file "bundle")
               (:file "delivery")))


(asdf:defsystem :alien-works-delivery/appimage
  :description "Plugin for alien-works-delivery to deliver bundles in AppImage format"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:uiop :asdf :alexandria :alien-works-delivery)
  :license "MIT"
  :pathname "appimage/"
  :components ((:file "appimage")))


(asdf:defsystem :alien-works-delivery/msix
  :description "Plugin for alien-works-delivery to deliver bundles in MSIX format"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:uiop :asdf :alexandria :alien-works-delivery)
  :license "MIT"
  :pathname "msix/"
  :components ((:file "msix")))


(asdf:defsystem :alien-works-delivery/archive
  :description "Plugin for alien-works-delivery to deliver binary archives"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:uiop :asdf :alexandria :alien-works-delivery)
  :license "MIT"
  :pathname "archive/"
  :components ((:file "archive")))
