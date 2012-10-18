(defstruct (grid-maker
   (:constructor create-grid-maker (name function))
   (:print-function
       (lambda (grid-maker s k)
           (format s "~A" (grid-maker-name grid-maker)) ) ) )
   (name NIL :type simple-string)
   (function 'random-grid-maker-default) )

(defstruct (data-set
   (:include grid-maker)
   (:print-function
       (lambda (data-set s k)
           (format s "~A ~A" (grid-maker-name data-set) (data-set-desc data-set)) ) ) )
   (desc NIL :type simple-string)
   (path NIL :type simple-string) )

(defun create-data-set(name path desc)
   (make-data-set
      :name name
      :path path
      :desc desc
      :function #'(lambda (data-set) (parse-data-set (data-set-path data-set))) ) )

(defun random-grid-maker(grid-maker)
   (format t "Please enter the number of colors, columns and rows of the board.~%")
   (let ((colors (get-positive-int "Please enter a positive number of colors...~%"))
         (rows (get-positive-int "Please enter a positive number of rows...~%"))
         (cols (get-positive-int "Please enter a positive number of columns...~%")))
      (random-fill (make-grid :cols cols :rows rows :colors colors)) ) )

(defun random-grid-maker-default(grid-maker)
   (random-fill (make-grid)) )

(defun prompt-for-grid-maker()
 (create-grid-maker "Data-set, user-specified path" 'grid-from-path))

(defun grid-from-path(grid-maker)
   (format t "~%Please enter the path for the data-set:~%")
   (parse-data-set
     (read-valid
      #'(lambda (choice) (probe-file choice))
      "That file does not exist...~%" ) ) )

(defun create-grid(grid-maker)
   (funcall (grid-maker-function grid-maker) grid-maker) )
