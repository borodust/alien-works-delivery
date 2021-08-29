(cl:defpackage :alien-works-delivery
  (:use :cl :alexandria :alien-works-delivery-util)
  (:export #:assemble-delivery-bundle

           #:*delivery-bundle-directory*
           #:make-delivery-bundle
           #:prepare-delivery-bundle
           #:delivery-bundle-foreign-library-directory
           #:delivery-bundle-asset-directory
           #:delivery-bundle-executable-path
           #:delivery-bundle-assembler-parameters
           #:write-delivery-bundle-assembler-source)
  (:local-nicknames (:awdb :alien-works-delivery-bundle)))
