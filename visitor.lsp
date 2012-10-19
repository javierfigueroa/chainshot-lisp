;; Visitor constructor and methods...
(defstruct (visitor)
   (board NIL :type array) )

(defun create-visitor(grid)
  "Makes an array the same size as grid, specifically for visiting."
  (make-visitor :board (make-array 
      (multiple-value-list (get-grid-dimensions grid))
      :fill-pointer NIL) ) )

(defun visited-p(visitor bead &key (test #'null))
  "Returns whether bead has already been visitor (in the given visitor array)."
  (multiple-value-bind (row col) (translate bead)
      (funcall test 
        (aref (visitor-board visitor) col row) ) ) )

(defun visit(visitor bead &optional (value T))
  "Marks the cell corresponding to bead as being visitor."
  (multiple-value-bind (row col) (translate bead)
    (setf (aref (visitor-board visitor) col row) value) ) )

(defun translate(bead)
  (values
    (1- (car bead))
    (1- (cdr bead)) ) )