(defstruct (grid-builder
   (:constructor create-grid-builder (name function))
   (:print-function
       (lambda (grid-builder s k)
           (format s "~A" (grid-builder-name grid-builder)) ) ) )
   (name NIL :type simple-string)
   (function 'read-grid-from-path) )

(defstruct (data-set
   (:include grid-builder)
   (:print-function
       (lambda (data-set s k)
           (format s "~A ~A" (grid-builder-name data-set) (data-set-desc data-set)) ) ) )
   (desc NIL :type simple-string)
   (path NIL :type simple-string) )

(defun create-data-set(name path desc)
   (make-data-set
      :name name
      :path path
      :desc desc
      :function #'(lambda (data-set) (parse-data-set (data-set-path data-set))) ) )

(defun read-grid-from-path()
 (create-grid-builder "File" 'grid-from-path))

(defun grid-from-path(grid-builder)
   (format t "~%Please enter the name of the grid file:~%")
   (parse-data-set
     (read-valid
      #'(lambda (choice) (probe-file choice))
      "File does not exist~%" ) ) )

(defun create-grid(grid-builder)
   (funcall (grid-builder-function grid-builder) grid-builder) )
