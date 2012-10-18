;; Guest bead, a bead that land on a new spot in the grid.
(defstruct (guest)
   (board NIL :type array) )

(defun create-guest(grid)
  "Makes an array the same size as grid, specifically for visiting."
  (make-guest :board (make-array 
      (multiple-value-list (get-dimensions grid))
      :fill-pointer NIL) ) )

(defun get-guest-bead(guest bead &key (test #'null))
  "Returns whether piece has already been visitor (in the given visitor array)."
  (multiple-value-bind (row col) (translate bead)
      (funcall test 
        (aref (guest-board guest) col row) ) ) )

(defun set-guest(guest bead &optional (value T))
  "Marks the cell corresponding to piece as being visitor."
  (multiple-value-bind (row col) (translate bead)
    (setf (aref (guest-board guest) col row) value) ) )

(defun translate(bead)
  (values
    (1- (car bead))
    (1- (cdr bead)) ) )