(cl:defpackage :alien-works-delivery
  (:use :cl :alexandria :alien-works-delivery-util)
  (:export #:assemble-delivery-bundle

           #:*delivery-bundle-directory*
           #:make-delivery-bundle
           #:prepare-delivery-bundle
           #:delivery-bundle-foreign-library-directory
           #:delivery-bundle-asset-directory
           #:delivery-bundle-executable-path
           #:delivery-bundle-build-features
           #:delivery-bundle-assembler-parameters
           #:write-delivery-bundle-assembler-source

           #:defbundle
           #:bundle-assets
           #:bundle-system-name
           #:bundle-entry-point

           #:bundle-file
           #:bundle-file-source
           #:bundle-file-destination
           #:bundle-library)
  (:local-nicknames (:awdb :alien-works-delivery-bundle)))
