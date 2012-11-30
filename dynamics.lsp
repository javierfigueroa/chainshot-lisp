;; This file contains functions of the dynamics of the game

(defun play-as-human(player grid playing-time &optional (moves (next-move player grid)) (previous-moves '()))
  "This function contains the main logic that's executed for a human player. It checks if the player's move is valid
  and if so it executes the move while checking if the move has reached an ending point in the game."
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-output grid NIL (reverse previous-moves)) )
      ((check-for-invalid-move grid move)
        (feedback "~A is not a valid move!~%~%" move)
        (play-as-human player grid playing-time (next-move player grid) previous-moves) )
      (T
        (let ((new-grid (move-bead grid (car move) (cdr move))))
          (if
            (or (is-solved new-grid)(is-not-solved new-grid))
              (create-output new-grid NIL (reverse (cons move previous-moves)))
            (play-as-human player new-grid playing-time (next-move player new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )

(defun human-move(grid)
   "Prompts human player for a move."
   (print grid)
   (format t "~%Enter a row and a column (e.g. 2 1):~%")
   (list
     (let ((rows (grid-rows grid)) (cols (grid-cols grid)))
       (cons
         (validate-input
           #'(lambda (row) (and (integerp row) (is-between row 0 (1+ rows))))
           "Enter a valid row number~%" )
         (validate-input
           #'(lambda (col) (and (integerp col) (is-between col 0 (1+ cols))))
           "Enter a valid column number~%" ) ) ) ) )

(defun first-combo-first-searched(user grid deadline &optional (moves (next-move user grid)) (previous-moves '()))
 (let ((move (car moves)))
   (cond
     ((null move)
       (create-output grid NIL (reverse previous-moves)) )
     ((check-for-invalid-move grid move)
       (feedback "~A is an invalid move!~%Please try again...~%" move)
       (first-combo-first-searched user grid deadline (cdr moves) previous-moves) )
     (T
       (let ((new-grid (move-bead grid (car move) (cdr move))))
         (if
           (or (is-solved new-grid) (is-not-solved new-grid))
             (create-output new-grid NIL (reverse (cons move previous-moves)))
           (first-combo-first-searched user new-grid deadline (next-move user new-grid)
              (cons move previous-moves) ) ) ) ) ) ) )


(defun depth-first(user grid deadline &optional (moves (next-move user grid)) (result (create-output grid)) (previous-moves '()))
  (cond
    ((> (now) deadline)
      (with-time-out
        (first-combo-first-searched user grid deadline (next-move user grid) previous-moves) ) )
    ((or (null moves) (is-not-solved grid) (is-solved grid))
      (create-output grid result previous-moves) )
    (T
      (let ((move (car moves)))
        (if (check-for-invalid-move grid move)
          (depth-first user grid deadline (cdr moves) result previous-moves)
          (let*
            ((new-grid (move-bead (copy-grid grid) (car move) (cdr move)))
             (result
               (depth-first user new-grid deadline (next-move user new-grid) result (cons move previous-moves)) ) )
            (if (or (output-winner result) (output-time-out result))
              result
              ; TODO : I reversed the list of upcoming moves after each failure
              (depth-first user grid deadline (reverse (cdr moves)) result previous-moves) ) ) ) ) ) ) )


(defun single-move(grid)
 (let ((board (grid-board grid)))
   (reverse
     (loop
       for row from 1 below (1+ (grid-rows grid))
       do NIL
       append (combo-row-finder board row) ) ) ) )

(defun combo-row-finder(board &optional (row 1) (col 1) (max-col (list-length board)) (combo-color NIL) (bucket '()))
 (single-combo-logic board row col bucket max-col
   (get-cell-color-for-board board row col)
   combo-color
   (get-cell-color-for-board board row (1+ col))
   (get-cell-color-for-board board (1+ row) col)
   (get-cell-color-for-board board (1- row) col)
   (get-cell-color-for-board board (1+ row) (1- col)) ) )    

(defun single-combo-logic(board row col bucket max-col color left right up down upper-left)
 (if (> col max-col)
   bucket
   (combo-row-finder board row (1+ col) max-col
     color
     (if
       (and
         (or (equalp right color) (equalp up color))
         (not (or (equalp left color) (equalp down color) (and (equalp upper-left color) (equalp up color)))) )
       (cons (cons row col) bucket)
       bucket ) ) ) )


;; Helper methods for the above functions

(defun check-for-invalid-move(grid move)
    (or (null (>= (length (find-beads grid (car move) (cdr move))) 3)) (not (get-bead-group grid move)))) 

(defun get-bead-group(grid move)
  (if (null (get-cell-color-for-grid grid (car move) (cdr move)))
    (logger "Bad color for ~A.~%" move)
    (loop for neighbor in (get-neighbor-beads move) do NIL
      when (is-same-color-bead (grid-board grid) move neighbor)
        return T ) ) )

(defun is-same-color-bead(board bead other) 
   (equalp
     (get-cell-color-for-board board (car bead) (cdr bead))
     (get-cell-color-for-board board (car other) (cdr other)) ) )

(defun find-beads(grid row col) 
  (if (null (get-cell-color-for-grid grid row col))
    '()
    (cons (cons row col)
      (find-neighbor-beads
        (grid-board grid)
        (cons row col)
        (create-dead grid) ) ) ) )

(defun get-neighbor-beads(bead)  
   "Get neighbor beads adjacent to given bead."
   (let ((row (car bead)) (col (cdr bead)))
      (cons (cons (1+ row) col)
         (cons (cons (1- row) col) 
            (cons (cons row (1+ col)) 
               (cons (cons row (1- col)) '()) ) ) ) ) )

(defun find-neighbor-beads(board bead dead &optional (beads '())) 
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
      (values (clean-grid grid) (list-length beads))
      (values grid 0) ) ) )
