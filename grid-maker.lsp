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
  (let ((grid-makers
    (create-hash-table
     (create-data-set "Data-set  1" "ds1.cs"  "( 5 x  5, 4 colors)")
     (create-data-set "Data-set  2" "ds2.cs"  "( 5 x  5, 3 colors)")
     (create-data-set "Data-set  3" "ds3.cs"  "( 5 x  5, 4 colors)")
     (create-data-set "Data-set  4" "ds4.cs"  "( 5 x  5, 5 colors)")
     (create-data-set "Data-set  5" "ds5.cs"  "( 7 x  7, 3 colors)")
     (create-data-set "Data-set  6" "ds6.cs"  "( 7 x  7, 4 colors)")
     (create-data-set "Data-set  7" "ds7.cs"  "( 7 x  7, 5 colors)")
     (create-data-set "Data-set  8" "ds8.cs"  "(10 x 10, 3 colors)")
     (create-data-set "Data-set  9" "ds9.cs"  "(10 x 10, 5 colors)")
     (create-data-set "Data-set 10" "ds10.cs" "(20 x 20, 3 colors)")
     (create-data-set "Data-set 11" "ds11.cs" "(20 x 20, 5 colors)")
     (create-data-set "Data-set 12" "ds12.cs" "(20 x 20, 5 colors)")
     (create-grid-maker "Data-set, user-specified path" 'grid-from-path)
     (create-grid-maker "Random fill (5 x 5, 3 colors)" 'random-grid-maker-default)
     (create-grid-maker "Random fill, user-specified size" 'random-grid-maker) ) ))

   (format t "~%Please choose how you would like to construct the board:~%")
   (maphash
     #'(lambda (key entry)
        (format t "  ~A~A : ~A~%" (if (> key 9) "" " ") key entry) )
    grid-makers)
   (gethash
      (read-valid #'(lambda (choice) (and (numberp choice) (not (null (gethash choice grid-makers))))))
      grid-makers) ) )

(defun grid-from-path(grid-maker)
   (format t "~%Please enter the path for the data-set:~%")
   (parse-data-set
     (read-valid
      #'(lambda (choice) (probe-file choice))
      "That file does not exist...~%" ) ) )

(defun create-grid(grid-maker)
   (funcall (grid-maker-function grid-maker) grid-maker) )
