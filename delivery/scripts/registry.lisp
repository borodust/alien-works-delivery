(cl:in-package :cl-user)

;;
;; INPUT:
;;   *registry-paths*
;;

(require 'asdf)
(require 'uiop)
(loop for relative-path in *registry-paths*
      do (push (merge-pathnames relative-path
                                (uiop:pathname-directory-pathname *load-pathname*))
               asdf:*central-registry*))
