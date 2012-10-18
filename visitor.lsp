;; Visitor constructor and methods...
(defstruct (visitor)
   (board NIL :type array) )

(defun create-visitor(grid)
  "Makes an array the same size as grid, specifically for visiting."
  (make-visitor :board (make-array 
      (multiple-value-list (get-dimensions grid))
      :fill-pointer NIL) ) )

(defun visited-p(visitor piece &key (test #'null))
  "Returns whether piece has already been visitor (in the given visitor array)."
  (multiple-value-bind (row col) (translate piece)
      (funcall test 
        (aref (visitor-board visitor) col row) ) ) )

(defun visit(visitor piece &optional (value T))
  "Marks the cell corresponding to piece as being visitor."
  (multiple-value-bind (row col) (translate piece)
    (setf (aref (visitor-board visitor) col row) value) ) )

(defun translate(piece)
  (values
    (1- (car piece))
    (1- (cdr piece)) ) )