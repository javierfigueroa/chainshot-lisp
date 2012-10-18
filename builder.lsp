(defstruct (builder
   (:constructor create-builder (name function))
   (:print-function
       (lambda (builder s k)
           (format s "~A" (builder-name builder)) ) ) )
   (name NIL :type simple-string)
   (function 'grid-from-path) )

(defun prompt-for-grid-maker() 
     (create-builder "Path" 'grid-from-path) )


(defun grid-from-path(builder)
   (format t "~%Enter a path for the data set:~%")
   (parse-data-set
     (validate-input
      #'(lambda (choice) (probe-file choice))
      "Provided file does not exist!~%" ) ) )

(defun build-grid(builder)
   (funcall (builder-function builder) builder) )
