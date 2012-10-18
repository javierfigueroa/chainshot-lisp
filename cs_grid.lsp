;; Grid analysis:

(defun random-fill(grid)
   (setf (grid-board grid)
      (fill-grid
         (grid-rows grid)
         (grid-cols grid)
         (grid-colors grid) ) )
   grid )

(defun fill-grid(rows length colors)
   "Returns a list of size rows x length.
    The contents will be distributed randomly some number of colors."
   (cond ((zerop rows) NIL)
	   ((append (list (loop for x from 0 below length collect (int-to-color (random colors))))
	       (fill-grid (1- rows) length colors))) ) )

(defun grid-has-combos(grid)
   "Returns T if the grid has any vertical combinations to be made."
   (reduce #'(lambda (x y) (or x y)) (mapcar 'has-pairs grid)) )

(defun has-pairs(line &optional (length (list-length line)))
   "Returns T if a given list/line has two-in-a-row of any non-null color."
   (cond
      ((or (zerop length) (null (car line)) (null (cdr line))) NIL)
      ((equalp (nth 0 line) (nth 1 line)) T)
      ((has-pairs (cdr line) (1- length))) ) )

(defun get-dimensions(grid)
   "Returns the grid's dimensions: rows AND length."
   (values (grid-rows grid) (grid-cols grid)) )

;; Grid manipulation:

(defun get-color(grid row col)
   "Returns the color of the cell at {row, col}."
   (cell-color (grid-board grid) row col) )

(defun cell-color(board row col)
   "Returns the color of the cell at {row, col}."
  (if (and (plusp row) (plusp col))
    (nth (1- row) (nth (1- col) board))
    NIL ) )

(defun set-color(grid row col val)
   "Updates cell color of grid at {row, col} to val."
   (setf (nth (1- row) (nth (1- col) (grid-board grid))) val) )

(defun pad(board rows length char)
   "Pads a list of lists to given rows and length with char."
   (cond ((<= rows 0) NIL)
         ((append (list (pad-row (car board) length char))
                  (pad (cdr board) (1- rows) length char))) ) )

(defun pad-row(row length char)
   "Pads row to length with char."
   (cond ((zerop length) NIL)
         ((null row) (cons char (pad-row '() (1- length) char)))
         ((cons (car row) (pad-row (cdr row) (1- length) char))) ) )

(defun compress-line(line)
   "Will compress the given list, removing all NIL values."
   (cond ((null line) NIL)
      ((null (car line)) (compress-line (cdr line)))
      ((cons (car line) (compress-line (cdr line)))) ) )

(defun pack-grid(grid)
  (setf
    (grid-board grid)
    (pad
      (compress (grid-board grid))
      (grid-rows grid)
      (grid-cols grid)
      NIL ) )
  grid )

(defun compress(board)
   "Modifies board by removing all cells with NIL as a value."
   (cond ((null board) '())
       ((null (car board)) (compress (cdr board)))
       ((let ((a (compress-line (car board)))
              (b (compress (cdr board))))
           (if (null a)
                b
                (cons a b))) ) ) )

(defun show-grid(grid &optional (s t))
   "Pretty-print function for a given grid.
    It uses the length and rows of the last constructed grid for sizing purposes."
   (let* ((length (grid-cols grid)) (rows (grid-rows grid)))
      ; showing to user involves padding the grid, reversing and transposing it
      (show-grid-inner
        (transpose
          (mapcar 'reverse
            (pad (compress (grid-board grid)) rows length  ".") ) )
         s (grid-cols grid))
      (format s "~%    ")
      (loop for x from 0 below rows do (format s "---"))
      (format s "~%    ")
      (loop for x from 1 below (min 10 (1+ rows)) do (format s "  ~A" x))
      (loop for x from 10 below (1+ rows) do (format s " ~A" x)) ) )

(defun show-grid-inner(board s index)
   "Pretty-print implementation for a list of columns."
   (when (not (atom board))
      (show-line (car board) s index)
      (show-grid-inner (cdr board) s (1- index))
      T ) )

(defun show-line(line s index)
   "Pretty-print implementation for a row (line)."
   (if (<= index 9)
       (format s "~% ~A |" index)
       (format s "~%~A |" index))
   (loop for x in line do
      (if (or (equalp "." x) (null x))
         (format s "  .")
         (format s "  ~A" x) ) ) )

(defun grid-count(grid)
  "Returns the number of cells left in grid."
  (reduce '+
    (mapcar
      (lambda (line) (list-length (remove NIL line)))
      (grid-board grid) ) ) )

(defun count-distinct(some-list)
   (list-length
      (remove-duplicates
         (remove NIL
            (reduce 'append some-list) ) ) ) )
