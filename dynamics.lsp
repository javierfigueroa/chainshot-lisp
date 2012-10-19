
(defun play-as-human(user grid deadline &optional (moves (next-move user grid)) (previous-moves '()))
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-result grid NIL (reverse previous-moves)) )
      ((check-for-duplicate-move grid move)
        (feedback "~A is an invalid move!~%Please try again...~%" move)
        (play-as-human user grid deadline (next-move user grid) previous-moves) )
      (T
        (let ((new-grid (do-move grid (car move) (cdr move))))
          (if
            (or (is-solved new-grid) (is-not-solved new-grid))
              (create-result new-grid NIL (reverse (cons move previous-moves)))
            (play-as-human user new-grid deadline (next-move user new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )


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

(defun check-for-duplicate-move(grid move)
  (not (get-bead-group grid move)) )

(defun get-bead-group(grid move)
  (if (null (get-bead-color grid (car move) (cdr move)))
    (verbose "Null color for ~A... already been played.~%" move)
    (loop for neighbor in (get-neighbors move) do NIL
      when (same-color-p (grid-board grid) move neighbor)
        return T ) ) )
