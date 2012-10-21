;; This file contains function related to the output IO, mostly printing the game statistics after the game is played

(defstruct (output (:print-function print-output) )
  (winner NIL)
  (loser NIL)
  (time-out NIL)
  (remaining NIL)
  (moves NIL :type list)
  (best NIL :type grid) )

(defun print-output(r s &rest rest)
  (format-output s (output-best r) (output-moves r) (output-winner r) (output-loser r) (output-time-out r)) )

(defun format-output(s best moves winner loser time-out)
  (cond
    (winner
      (format s "~%You're a winner!~%You made these moves:~%~A" moves)
       T )
    (T
      (cond
        (time-out (format s "~%Time is out, the best output was:  ") )
        (loser    (format s "~%You loose! the best output was:  ") )
        (T        (format s "~%Go again?  ") ) )
     (format s "~%~A~%~%Moves performed:~%~A~%" best moves) ) ) )

(defun create-output(grid &optional (previous-output NIL) (moves '()) (timeout NIL))
  (merge-outputs
    (make-output :best grid :moves moves 
      :winner (is-solved grid) :loser (is-not-solved grid) :remaining (grid-beads-left-count grid) :time-out timeout)
    previous-output ) )

(defun time-out(grid &optional (moves '()))
  (create-output grid NIL moves T) )

(defun merge-outputs(output other &key (scale 'scale-output))
  (if
    (or (null other)
      (< (funcall scale output) (funcall scale other)) )
    output
    other ) )

(defun scale-output(output)
  (output-remaining output) )
