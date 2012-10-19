
(defun play-as-human(user grid deadline &optional (moves (next-move user grid)) (previous-moves '()))
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-result grid NIL (reverse previous-moves)) )
      ((check-for-duplicate-move grid move)
        (feedback "~A is not a valid move!~%Try again...~%" move)
        (play-as-human user grid deadline (next-move user grid) previous-moves) )
      (T
        (let ((new-grid (move-bead grid (car move) (cdr move))))
          (if
            (or (is-solved new-grid) (is-not-solved new-grid))
              (create-result new-grid NIL (reverse (cons move previous-moves)))
            (play-as-human user new-grid deadline (next-move user new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )


(defun human-move(grid)
   (print grid)
   (format t "~%Enter the row and column for your next move:~%")
   (list
     (let ((rows (grid-rows grid)) (cols (grid-cols grid)))
       (cons
         (read-valid
           #'(lambda (row) (and (integerp row) (between row 0 (1+ rows))))
           "Enter a valid row number...~%" )
         (read-valid
           #'(lambda (col) (and (integerp col) (between col 0 (1+ cols))))
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
        (create-visitor grid) ) ) ) )

(defun get-neighbor-beads(bead) ;;from find-neighbor-bead and dynamics 
   "Get neighbor beads adjacent to given bead."
   (let ((row (car bead)) (col (cdr bead)))
      (cons (cons (1+ row) col)
         (cons (cons (1- row) col) 
            (cons (cons row (1+ col)) 
               (cons (cons row (1- col)) '()) ) ) ) ) )

(defun find-neighbor-beads(board bead visitor &optional (beads '())) ;; from top
  "Get beads neighboring bead with non-visited status and of the same color."
  (visit visitor bead)
  (loop
    for neighbor in (get-neighbor-beads bead) do NIL
    if (and (is-same-color-bead board bead neighbor) (visited-p visitor neighbor))
    append
     (cons neighbor
       (find-neighbor-beads board neighbor visitor (cons neighbor beads)) ) ) )

(defun move-bead(grid row col) ;; from dynamics
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

(defun set-cell-color(grid row col val) ;; from top
   "Set color for grid cell."
   (setf (nth (1- row) (nth (1- col) (grid-board grid))) val) )
