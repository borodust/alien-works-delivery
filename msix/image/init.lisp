(require 'asdf)

(load (merge-pathnames "quicklisp.lisp"
                       (uiop:pathname-directory-pathname *load-pathname*)))

(quicklisp-quickstart:install
 :dist-url "http://dist.awie.club/awie/0/awie.txt")
(ql-util:without-prompting
  (let ((init-file (ql:add-to-init-file)))
    (with-open-file (out init-file :direction :output
                                   :if-exists :append
                                   :element-type 'base-char
                                   :external-format :utf-8)
      (format out "~&#+quicklisp~%")
      (prin1 '(ql-util:without-prompting (ql:update-all-dists)) out))))
