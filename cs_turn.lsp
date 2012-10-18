;; Turn-based manipulation

(defun same-color-p(board piece other)
   "Returns whether piece has the same cell color as other."
   (equalp
     (cell-color board (car piece) (cdr piece))
     (cell-color board (car other) (cdr other)) ) )

(defun find-pieces(grid row col)
  (if (null (get-color grid row col))
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

(defun find-neighbors(board piece visitor &optional (pieces '()))
  "Returns a list of pieces neighboring piece having not yet been visited and having the same color."
  (visit visitor piece)
  (loop
    for neighbor in (get-neighbors piece) do NIL
    if (and (same-color-p board piece neighbor) (visited-p visitor neighbor))
    append
     (cons neighbor
       (find-neighbors board neighbor visitor (cons neighbor pieces)) ) ) )

(defun remove-pieces(grid pieces)
   "Sets cell color of all pieces to NIL. Returns NIL if no pieces were changed."
  (let ((valid (>= (list-length pieces) 2))) ; Valid combinations have size > 2
    (and valid
      (loop for p in pieces do
        (set-color grid (car p) (cdr p) NIL) ) )
    (if valid
      (values (pack-grid grid) (list-length pieces))
      (values grid 0) ) ) )

(defun do-move(grid row col)
  "Tries to make a move at {row, col}.
   Returns the resulting grid and whether the move was valid.
   This function is non-destructive on grid."
  (multiple-value-bind (new-grid pieces)
      (remove-pieces (copy-grid grid) (find-pieces grid row col))
    (when (zerop pieces) (format t "~%Invalid move: (~D, ~D)~%Previous grid: ~A" row col grid) (break))
    new-grid ) )
