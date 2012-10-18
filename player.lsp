(defstruct (player
  (:constructor create-player (move-function play-function has-feedback name))
  (:print-function
    (lambda (player s k)
      (format s "~A" (player-name player)) ) ) )
  (name NIL :type simple-string)
  (has-feedback NIL)
  move-function
  play-function )

(defun create-human-player()
	(create-player 'mutant-move 'play-human T "Human"))

(defun create-map(&rest values)
  (let ((map (make-hash-table))
        (index (list-length values)))
    (loop for val in (reverse values) do
      (setf (gethash index map) val)
      (setq index (1- index)) )
    map) )

(defun play-human(player grid deadline &optional (moves (next-move player grid)) (previous-moves '()))
  "This method defines the friendly version of the game play"
  (let ((move (car moves)))
    (cond
      ((null move)
        (create-result grid NIL (reverse previous-moves)) )
      ((check-invalid-move grid move)
        (feedback "~A is not valid!~%Try again.~%" move)
        (play-nice player grid deadline (next-move player grid) previous-moves) )
      (T
        (let ((new-grid (execute-move grid (car move) (cdr move))))
          (if
            (or (is-solved new-grid) (is-not-solved new-grid))
              (create-result new-grid NIL (reverse (cons move previous-moves)))
            (play-nice player new-grid deadline (next-move player new-grid)
               (cons move previous-moves) ) ) ) ) ) ) )

(defun next-move(player grid)
  "Calls the player's move function on the grid"
  (funcall (player-move-function player) grid) )

(defun player-play(player grid deadline)
  "Calls the player's play function on the grid"
  (funcall (player-play-function player) player grid deadline) )
