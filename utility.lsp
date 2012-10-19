;; Utility methods:

(defun clear-screen()
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun validate-input(test &optional (message "Invalid input, try again...~%"))
   (do ((val (read) (read)))
       ((funcall test val) val)
     (format t message) ) )

(defun is-between(num lower upper)
   (and (< lower num) (< num upper)) )

(defun transpose (x)
   "Do a transpose of x, such as in a matrix."
   (apply #'mapcar (cons #'list x)) )

(defun set-verbose(verbose)
  (setq *print-verbose* verbose) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun set-player-feedback(feedback)
  (setq *player-feedback* feedback) )

(defun feedback(&rest args)
  (if *player-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

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