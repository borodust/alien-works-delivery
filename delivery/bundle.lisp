(cl:in-package :alien-works-delivery)


(defvar *bundle-definition-registry* (make-hash-table :test 'equal))

(declaim (special *assets*
                  *system-name*))

(defun proper-bundle-name (bundle-name)
  (string-downcase (string bundle-name)) *bundle-definition-registry*)


(defclass bundle-definition ()
  ((system-name :initarg :system-name
                :initform (error ":system-name missing")
                :reader bundle-system-name)
   (entry-point :initarg :entry-point
                :initform nil
                :reader bundle-entry-point)
   (assets :initarg :assets
           :initform nil
           :reader bundle-assets)))


(defclass bundle-library () ())

(defmethod initialize-instance :after ((this bundle-library) &key definition))


(defclass bundle-file ()
  ((source :reader bundle-file-source)
   (destination :reader bundle-file-destination)))


(defmethod initialize-instance :after ((this bundle-file) &key definition)
  (with-slots (source destination) this
    (labels ((%system-path (relative)
               (asdf:system-relative-pathname *system-name*
                                              (uiop:relativize-pathname-directory relative)))
             (%asset-path (source)
               (if (listp source)
                   (if (eq (first source) :system)
                       (%system-path (second source))
                       (error "Unrecognized asset source: ~A" source))
                   source)))
      (destructuring-bind (file-source &key ((:to file-destination))) definition
        (setf source (%asset-path file-source)
              destination (if (emptyp file-destination)
                              "./"
                              (uiop:relativize-pathname-directory file-destination)))))))


(defun parse-library-definition (library-def)
  (push (make-instance 'bundle-library :definition library-def) *assets*))


(defun parse-file-definition (file-def)
  (push (make-instance 'bundle-file :definition file-def) *assets*))


(defun parse-group-definition (group-def)
  (parse-asset-definition (rest group-def)))


(defun parse-asset-definition (assets)
  (loop with libraries = (list)
        with files = (list)
        for (asset-type . asset-def) in assets
        do (ecase asset-type
             (:library (parse-library-definition asset-def))
             (:file (parse-file-definition asset-def))
             (:group (parse-group-definition asset-def)))))


(defmethod initialize-instance :after ((this bundle-definition) &key assets)
  (with-slots ((bundle-assets assets) system-name) this
    (let ((*assets* (list))
          (*system-name* system-name))
      (parse-asset-definition assets)
      (setf bundle-assets *assets*))))


(defun register-bundle-definition (bundle-name &rest initargs &key &allow-other-keys)
  (setf
   (gethash (proper-bundle-name bundle-name) *bundle-definition-registry*)
   (apply #'make-instance 'bundle-definition initargs)))


(defun find-bundle-definition (bundle-name)
  (if-let (bundle-def (gethash (proper-bundle-name bundle-name) *bundle-definition-registry*))
    bundle-def
    (error "Bundle with name `~A` not found" bundle-name)))


(defmacro defbundle (name-and-opts &body properties)
  (destructuring-bind (bundle-name)
      (ensure-list name-and-opts)
    (destructuring-bind (&key system entry-point assets)
        properties
      `(register-bundle-definition ',bundle-name
                                   :system-name ',(or system bundle-name)
                                   :entry-point ',entry-point
                                   :assets ',assets))))
