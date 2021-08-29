(cl:in-package :alien-works-delivery)


(defun register-system (table system)
  (labels ((%register (table paths system)
             (let* ((key-path (first paths))
                    (rest-paths (rest paths))
                    (node (gethash key-path table)))
               (unless node
                 (setf node (cons (make-hash-table :test 'equal) nil)
                       (gethash key-path table) node))
               (if rest-paths
                   (%register (car node) rest-paths system)
                   (push system (cdr node))))))
    (when-let ((path (asdf:system-source-directory system)))
      (%register table (rest (pathname-directory path)) system))))


(defun make-registry-table (systems)
  (let ((table (make-hash-table :test 'equal)))
    (loop for system in systems
          do (register-system table system))
    table))


(defun registry-root-systems (table)
  (loop for node being the hash-value of table
        for (inner . systems) = node
        if systems
          append systems
        else
          append (registry-root-systems inner)))


(defun %relative-systems (node relative-path)
  (destructuring-bind (inner . systems) node
    (append
     (when systems
       (loop with path = (reverse relative-path)
             for system in systems
             collect (cons (format nil "~{~A/~}" path) system)))
     (loop for key-path being the hash-key of inner
           append (%relative-systems (gethash key-path inner)
                                     (list* key-path relative-path))))))


(defun registry-relative-systems (table)
  (loop for node being the hash-value of table using (hash-key key-path)
        for (inner . systems) = node
        if systems
          append (%relative-systems node (list key-path))
        else
          append (registry-relative-systems inner)))
