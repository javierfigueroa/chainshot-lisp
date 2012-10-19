;; Grid analysis:


;; Grid manipulation:

(defun get-bead-color(grid row col) ;from dynamics
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

;; Turn-based manipulation

(defun same-color-p(board piece other)
   "Returns whether piece has the same cell color as other."
   (equalp
     (cell-color board (car piece) (cdr piece))
     (cell-color board (car other) (cdr other)) ) )

(defun find-beads(grid row col)
  (if (null (get-bead-color grid row col))
    '()
    (cons (cons row col)
      (find-neighbors
        (grid-board grid)
        (cons row col)
        (create-visitor grid) ) ) ) )

(defun get-neighbors(piece)
   "Returns a list of cons-cells, one for each of the 
   neighbors adjacent to given piece (being a cons of its row, col)."
   (let ((row (car piece)) (col (cdr piece)))
      (cons (cons (1+ row) col)
         (cons (cons (1- row) col) 
            (cons (cons row (1+ col)) 
               (cons (cons row (1- col)) '()) ) ) ) ) )

(defun find-neighbors(board piece visitor &optional (beads '()))
  "Returns a list of beads neighboring piece having not yet been visited and having the same color."
  (visit visitor piece)
  (loop
    for neighbor in (get-neighbors piece) do NIL
    if (and (same-color-p board piece neighbor) (visited-p visitor neighbor))
    append
     (cons neighbor
       (find-neighbors board neighbor visitor (cons neighbor beads)) ) ) )

(defun remove-beads(grid beads)
   "Sets cell color of all beads to NIL. Returns NIL if no beads were changed."
  (let ((valid (>= (list-length beads) 2))) ; Valid combinations have size > 2
    (and valid
      (loop for p in beads do
        (set-color grid (car p) (cdr p) NIL) ) )
    (if valid
      (values (pack-grid grid) (list-length beads))
      (values grid 0) ) ) )

(defun do-move(grid row col)
  "Tries to make a move at {row, col}.
   Returns the resulting grid and whether the move was valid.
   This function is non-destructive on grid."
  (multiple-value-bind (new-grid beads)
      (remove-beads (copy-grid grid) (find-beads grid row col))
    (when (zerop beads) (format t "~%Invalid move: (~D, ~D)~%Previous grid: ~A" row col grid) (break))
    new-grid ) )
