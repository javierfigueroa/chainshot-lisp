(defun execute-move(grid row col)
  "Executes a move based on a row and column value. Returns the new grid."
  (multiple-value-bind (new-grid beads)
      (remove-beads (copy-grid grid) (get-beads grid row col))
    (when (zerop beads) (format t "~%Invalid move: (~D, ~D)~%Previous grid: ~A" row col grid) (break))
    new-grid ) )

(defun remove-beads(grid beads)
   "Cleans given pieces list by setting them to NIL."
  (let ((valid (>= (list-length beads) 3))) ; Valid group of beads have size > 3
    (and valid
      (loop for bead in beads do
        (set-cell-color grid (car bead) (cdr bead) NIL) ) )
    (if valid
      (values (pack-grid grid) (list-length beads))
      (values grid 0) ) ) )

(defun get-beads(grid row col)
  (if (null (get-cell-color (grid-board grid) row col))
    '()
    (cons (cons row col)
      (find-neighbor-beads
        (grid-board grid)
        (cons row col)
        (create-guest grid) ) ) ) )

(defun find-neighbor-beads(board bead guest &optional (beads '()))
  "Gets a list of beads neighboring a bead that has not yet been visited with the same color."
  (set-guest guest bead)
  (loop
    for neighbor in (get-neighbor-beads bead) do NIL
    if (and (is-same-color-bead board bead neighbor) (get-guest-bead guest neighbor))
    append
     (cons neighbor
       (find-neighbor-beads board neighbor guest (cons neighbor beads)) ) ) )

(defun get-neighbor-beads(bead)
   "Gets a list of beads neighboring a specific bead."
   (let ((row (car bead)) (col (cdr bead)))
      (cons (cons (1+ row) col)
         (cons (cons (1- row) col) 
            (cons (cons row (1+ col)) 
               (cons (cons row (1- col)) '()) ) ) ) ) )

(defun is-same-color-bead(board bead other)
   "Checks if the bead has the same color as the other."
   (equalp
     (get-cell-color board (car bead) (cdr bead))
     (get-cell-color board (car other) (cdr other)) ) )