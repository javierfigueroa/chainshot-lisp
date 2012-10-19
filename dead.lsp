(defstruct (dead)
   (board NIL :type array) )

(defun create-dead(grid)
  "Build a copy of the array for NIL(dead) beads."
  (make-dead :board (make-array 
      (multiple-value-list (get-grid-dimensions grid))
      :fill-pointer NIL) ) )

(defun is-dead(dead bead &key (test #'null))
  "Check if bead is NIL(dead) in array."
  (multiple-value-bind (row col) (translate bead)
      (funcall test 
        (aref (dead-board dead) col row) ) ) )

(defun mark-dead(dead bead &optional (value T))
  "Set bead as NIL (dead)."
  (multiple-value-bind (row col) (translate bead)
    (setf (aref (dead-board dead) col row) value) ) )

(defun translate(bead)
  (values
    (1- (car bead))
    (1- (cdr bead)) ) )