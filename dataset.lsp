
(defstruct (data-set
   (:include builder)
   (:print-function
       (lambda (data-set s)
           (format s "~A ~A" (builder-name data-set) (data-set-desc data-set)) ) ) )
   (desc NIL :type simple-string)
   (path NIL :type simple-string) )


(defun create-data-set(name path desc)
   (make-data-set
      :name name
      :path path
      :desc desc
      :function #'(lambda (data-set) (parse-data-set (data-set-path data-set))) ) )

(defun split-strings(strings)
   (loop for string in strings
      collect
      (loop for i from 0 below (array-dimension string 0) 
         collect
         (aref string i) ) ) )

(defun get-grid-from-board(board)
   (let ((width (list-length board))
         (length (list-length (car board))) )
      (make-grid :rows width :cols length :board board :colors (count-distinct board)) ) )

(defun safe-read-line(is)
   (read-line is NIL NIL) )

(defun parse-data-set(data_set)
   (with-open-file (is data_set :direction :input
                    :if-does-not-exist nil)
      (let ((strings NIL))
         (do ((line (safe-read-line is) (safe-read-line is)))
             ((or (string= "" line) (not line)))
            (setq strings (cons line strings)) )
         (get-grid-from-board (transpose (split-strings strings))) ) ) )