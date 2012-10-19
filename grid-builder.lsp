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

(defun parse-data-set(data_set)
   (with-open-file (is data_set :direction :input
                    :if-does-not-exist nil)
      (let ((strings NIL))
         (do ((line (safe-read-line is) (safe-read-line is)))
             ((or (string= "" line) (not line)))
            (setq strings (cons line strings)) )
         (grid-from-board (transpose (split-strings strings))) ) ) )

(defun safe-read-line(is)
   (read-line is NIL NIL) )

(defun split-strings(strings)
   (loop for string in strings
      collect
      (loop for i from 0 below (array-dimension string 0) 
         collect
         (aref string i) ) ) )

(defun read-grid-from-path()
 (create-grid-builder "File" 'grid-from-path))

(defun grid-from-path(grid-builder)
   (format t "~%Enter the name of the grid file:~%")
   (parse-data-set
     (validate-input
      #'(lambda (choice) (probe-file choice))
      "File does not exist~%" ) ) )

(defun grid-from-board(board)
   (let ((width (list-length board))
         (length (list-length (car board))) )
      (make-grid :rows width :cols length :board board :colors (count-distinct-beads board)) ) )

(defun create-grid(grid-builder)
   (funcall (grid-builder-function grid-builder) grid-builder) )

(defun count-distinct-beads(some-list)
   (list-length
      (remove-duplicates
         (remove NIL
            (reduce 'append some-list) ) ) ) )
