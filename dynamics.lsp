(defun play-as-human(player grid deadline &optional (moves (next-move player grid)) (previous-moves '()))
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-output grid NIL (reverse previous-moves)) )
      ((check-for-duplicate-move grid move)
        (feedback "~A is not a valid move!~%Try again...~%" move)
        (play-as-human player grid deadline (next-move player grid) previous-moves) )
      (T
        (let ((new-grid (move-bead grid (car move) (cdr move))))
          (if
            (or (is-not-solved new-grid)(is-solved new-grid) )
              (create-output new-grid NIL (reverse (cons move previous-moves)))
            (play-as-human player new-grid deadline (next-move player new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )

(defun human-move(grid)
   (print grid)
   (format t "~%Enter the row and column for your next move:~%")
   (list
     (let ((rows (grid-rows grid)) (cols (grid-cols grid)))
       (cons
         (validate-input
           #'(lambda (row) (and (integerp row) (is-between row 0 (1+ rows))))
           "Enter a valid row number...~%" )
         (validate-input
           #'(lambda (col) (and (integerp col) (is-between col 0 (1+ cols))))
           "Enter a valid column number...~%" ) ) ) ) )

(defun check-for-duplicate-move(grid move)
  (not (get-bead-group grid move)) )

(defun get-bead-group(grid move)
  (if (null (get-cell-color-for-grid grid (car move) (cdr move)))
    (verbose "Null color for ~A... already been played.~%" move)
    (loop for neighbor in (get-neighbor-beads move) do NIL
      when (is-same-color-bead (grid-board grid) move neighbor)
        return T ) ) )

(defun is-same-color-bead(board bead other) ;; from dynamics
   (equalp
     (get-cell-color-for-board board (car bead) (cdr bead))
     (get-cell-color-for-board board (car other) (cdr other)) ) )

(defun find-beads(grid row col) ;; from move-bead
  (if (null (get-cell-color-for-grid grid row col))
    '()
    (cons (cons row col)
      (find-neighbor-beads
        (grid-board grid)
        (cons row col)
        (create-dead grid) ) ) ) )

(defun get-neighbor-beads(bead) ;;from find-neighbor-bead and dynamics 
   "Get neighbor beads adjacent to given bead."
   (let ((row (car bead)) (col (cdr bead)))
      (cons (cons (1+ row) col)
         (cons (cons (1- row) col) 
            (cons (cons row (1+ col)) 
               (cons (cons row (1- col)) '()) ) ) ) ) )

(defun find-neighbor-beads(board bead dead &optional (beads '())) ;; from top
  "Get beads neighboring bead with dead status and of the same color."
  (mark-dead dead bead)
  (loop
    for neighbor in (get-neighbor-beads bead) do NIL
    if (and (is-same-color-bead board bead neighbor) (is-dead dead neighbor))
    append
     (cons neighbor
       (find-neighbor-beads board neighbor dead (cons neighbor beads)) ) ) )

(defun move-bead(grid row col) 
  "Move bead."
  (multiple-value-bind (new-grid beads)
      (remove-beads (copy-grid grid) (find-beads grid row col))
    (when (zerop beads) (format t "~%Bad move: (~D, ~D)~%Previous grid: ~A" row col grid) (break))
    new-grid ) )

(defun remove-beads(grid beads)
   "Remove cell color of all beads to NIL."
  (let ((valid (>= (list-length beads) 3))) ; Valid combinations have size > 3
    (and valid
      (loop for p in beads do
        (set-cell-color grid (car p) (cdr p) NIL) ) )
    (if valid
      (values (arrange-grid grid) (list-length beads))
      (values grid 0) ) ) )
