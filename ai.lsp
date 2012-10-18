;;; "next-move" interface: user grid
;;; should return a list consisting of all suggested moves

(defun human-move(grid)
   (print grid)
   (format t "~%Please enter the row and column of your next move:~%")
   (list
     (let ((rows (grid-rows grid)) (cols (grid-cols grid)))
       (cons
         (read-valid
           #'(lambda (row) (and (integerp row) (between row 0 (1+ rows))))
           "Please enter a valid row number...~%" )
         (read-valid
           #'(lambda (col) (and (integerp col) (between col 0 (1+ cols))))
           "Please enter a valid column number...~%" ) ) ) ) )


(defun single-move-per-combo(grid)
  (let ((board (grid-board grid)))
    (reverse
      (loop
        for row from 1 below (1+ (grid-rows grid))
        do NIL
        append (combo-row-finder board row) ) ) ) )

(defun combo-row-finder(board &optional (row 1) (col 1) (max-col (list-length board)) (combo-color NIL) (bucket '()))
  (single-combo-logic board row col bucket max-col
    (cell-color board row col)
    combo-color
    (cell-color board row (1+ col))
    (cell-color board (1+ row) col)
    (cell-color board (1- row) col)
    (cell-color board (1+ row) (1- col)) ) )    

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

(defun shallow-moves-list(grid)
  (let
    ((moves (single-move-per-combo grid))
     (minimum-grid-count-to-trim 200)
     (maximum-moves 30) )
    (last moves maximum-moves) ) )

;;; "play" interface: user grid deadline moves
;;; Note: calling "user-play" can allow for the algorithm to be updated.

(defun first-combo-first-searched(user grid deadline &optional (moves (next-move user grid)) (previous-moves '()))
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-result grid NIL (reverse previous-moves)) )
      ((duplicate-move-p grid move)
        (feedback "~A is an invalid move!~%Please try again...~%" move)
        (first-combo-first-searched user grid deadline (cdr moves) previous-moves) )
      (T
        (let ((new-grid (do-move grid (car move) (cdr move))))
          (if
            (or (winnerp new-grid) (loserp new-grid))
              (create-result new-grid NIL (reverse (cons move previous-moves)))
            (first-combo-first-searched user new-grid deadline (next-move user new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )


(defun user-friendly(user grid deadline &optional (moves (next-move user grid)) (previous-moves '()))
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-result grid NIL (reverse previous-moves)) )
      ((duplicate-move-p grid move)
        (feedback "~A is an invalid move!~%Please try again...~%" move)
        (user-friendly user grid deadline (next-move user grid) previous-moves) )
      (T
        (let ((new-grid (do-move grid (car move) (cdr move))))
          (if
            (or (winnerp new-grid) (loserp new-grid))
              (create-result new-grid NIL (reverse (cons move previous-moves)))
            (user-friendly user new-grid deadline (next-move user new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )



(defun depth-first(user grid deadline &optional (moves (next-move user grid)) (result (create-result grid)) (previous-moves '()))
  (cond
    ((> (now) deadline)
      (with-time-out
        (first-combo-first-searched user grid deadline (next-move user grid) previous-moves) ) )
    ((or (null moves) (winnerp grid) (loserp grid))
      (create-result grid result previous-moves) )
    (T
      (let ((move (car moves)))
        (if (duplicate-move-p grid move)
          (depth-first user grid deadline (cdr moves) result previous-moves)
          (let*
            ((new-grid (do-move (copy-grid grid) (car move) (cdr move)))
             (result
               (depth-first user new-grid deadline (next-move user new-grid) result (cons move previous-moves)) ) )
            (if (or (result-winner result) (result-time-out result))
              result
              ; TODO : I reversed the list of upcoming moves after each failure
              (depth-first user grid deadline (reverse (cdr moves)) result previous-moves) ) ) ) ) ) ) )

(defun duplicate-move-p(grid move)
  "The single-move-per-combo algorithm has the particular case when it fails.
   This function verifies that the previous moves didn't overlap the given move (happens in less than 1% of all cases)."
  (not (combo-p grid move)) )

(defun combo-p(grid move)
  (if (null (get-color grid (car move) (cdr move)))
    (verbose "Null color for ~A... already been played.~%" move)
    (loop for neighbor in (get-neighbors move) do NIL
      when (same-color-p (grid-board grid) move neighbor)
        return T ) ) )
