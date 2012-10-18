; result.lsp
; Defines a "result" struct for storing search result data:
; - "Best" grid found.
; - search-tree resulting in the above grid.
; - any number of statuses/errors...
; "Winner", "Dead-end", "Loser", "Invalid move performed", ...

(defun now NIL (get-internal-run-time))

(defun time-format(time) (float (/ time internal-time-units-per-second)))

(defun print-time(time) (format t "~F sec." (time-format time)))

(defstruct (result (:print-function print-result) )
  (winner NIL)
  (loser NIL)
  (time-out NIL)
  (remaining NIL)
  (moves NIL :type list)
  (best NIL :type grid) )

(defun print-result(r s &rest rest)
  (format-result s (result-best r) (result-moves r) (result-winner r) (result-loser r) (result-time-out r)) )

(defun format-result(s best moves winner loser time-out)
  (cond
    (winner
      (format s "~%You won!~%Your solution took the following steps:~%~A" moves)
       T )
    (T
      (cond
        (time-out (format s "~%Time ran out, but the best result is:  ") )
        (loser    (format s "~%You lost! Your best result was:  ") )
        (T        (format s "~%You're not quite done, are you?  ") ) )
     (format s "~%~A~%~%Moves taken:~%~A~%" best moves) ) ) )

(defun create-result(grid &optional (previous-result NIL) (moves '()) (timeout NIL))
  (merge-results
    (make-result :best grid :moves moves 
      :winner (winnerp grid) :loser (loserp grid) :remaining (grid-count grid) :time-out timeout)
    previous-result ) )

(defun set-timed-out(result)
  (setf (result-time-out result) T) )

(defun time-out(grid &optional (moves '()))
  (create-result grid NIL moves T) )

(defun with-time-out(result)
  (set-timed-out result)
  result )

(defun merge-results(result other &key (scale 'scale-result))
  (if
    (or (null other)
      (< (funcall scale result) (funcall scale other)) )
    result
    other ) )

; lower is better
(defun scale-result(result)
  (result-remaining result) )

