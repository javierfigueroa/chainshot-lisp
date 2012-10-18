(defun split-strings(strings)
   (loop for string in strings
      collect
      (loop for i from 0 below (array-dimension string 0) 
         collect
         (aref string i) ) ) )

(defun grid-from-board(board)
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
         (grid-from-board (transpose (split-strings strings))) ) ) )