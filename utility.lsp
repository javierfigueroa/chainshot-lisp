;; Utility methods:

(defun clear-screen()
  "Tries to clear the screen/shell window where the current program is executing."
  #+WIN32 (run-shell-command "cls")
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun int-to-color(color)
   (int-char (+ (char-code #\A) color)) )

(defun create-hash-table(&rest values)
  (let ((ht (make-hash-table))
        (index (list-length values)))
    (loop for val in (reverse values) do
      (setf (gethash index ht) val)
      (setq index (1- index)) )
    ht) )

(defun read-valid(test &optional (message "Please try again...~%"))
   (do ((val (read) (read)))
       ((funcall test val) val)
     (format t message) ) )

(defun between(num lower upper)
   (and (< lower num) (< num upper)) )

(defun get-positive-int(message)
  (read-valid
     #'(lambda (x)
        (and (integerp x) (> x 0)) )
   message ) )

(defun transpose (x)
   "Returns a transpose of x (like a matrix).
    The row width of x' will be the minimum of the column lengths of x.
    The column length of x' will be the minimum of the row widths of x."
   (apply #'mapcar (cons #'list x)) )

(defun verbose-off()
  (setq *print-verbose* NIL) )

(defun verbose-on()
  (setq *print-verbose* T) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun set-user-feedback(feedback)
  (setq *user-feedback* feedback) )

(defun feedback(&rest args)
  (if *user-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )


(defun split-strings(strings)
   (loop for string in strings
      collect
      (loop for i from 0 below (array-dimension string 0) 
         collect
         (aref string i) ) ) )

(defun grid-from-board(board)
   (let ((width (list-length board))
         (length (list-length (car board))) )
      (make-grid :rows width :cols length :board board :colors (count-distinct-beads board)) ) )

(defun count-distinct-beads(some-list)
   (list-length
      (remove-duplicates
         (remove NIL
            (reduce 'append some-list) ) ) ) )

;; checked start

(defun get-cell-color-for-grid(grid row col) ;from dynamics
   "Get color of the cell by grid."
   (get-cell-color-for-board (grid-board grid) row col) )

(defun get-cell-color-for-board(board row col) ;from top
   "Get color of the cell by board."
  (if (and (plusp row) (plusp col))
    (nth (1- row) (nth (1- col) board))
    NIL ) )

(defun add-padding(board rows length char) ;; from main
   (cond ((<= rows 0) NIL)
         ((append (list (add-row-padding (car board) length char))
                  (add-padding (cdr board) (1- rows) length char))) ) )

(defun add-row-padding(row length char) ;; from top
   "Pads row to length with char."
   (cond ((zerop length) NIL)
         ((null row) (cons char (add-row-padding '() (1- length) char)))
         ((cons (car row) (add-row-padding (cdr row) (1- length) char))) ) )


(defun arrange-grid(grid) ;; from below (remove beads)
  (setf
    (grid-board grid)
    (add-padding
      (clean-board (grid-board grid))
      (grid-rows grid)
      (grid-cols grid)
      NIL ) )
  grid )

(defun clean-board(board) ;; from main and from top
   "Removes cell with NIL in the board."
   (cond ((null board) '())
       ((null (car board)) (clean-board (cdr board)))
       ((let ((a (clean-line (car board)))
              (b (clean-board (cdr board))))
           (if (null a)
                b
                (cons a b))) ) ) )

(defun clean-line(line) ;; from top
   "Removes NIL values from the line."
   (cond ((null line) NIL)
      ((null (car line)) (clean-line (cdr line)))
      ((cons (car line) (clean-line (cdr line)))) ) )

;; checked end

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