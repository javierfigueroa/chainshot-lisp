(defun get-playing-time()
  (format t "~%Thanks for asking me to play!~%About how long should I search? (in seconds)~%")
  (let 
    ((search-time-seconds
       (read-valid
        #'(lambda (choice)
            (and (realp choice) (plusp choice)) ) ) ))
    (+
      (now)
      (* search-time-seconds internal-time-units-per-second) ) ) )

(defun get-deadline(user)
  (if (user-feedback user)
    60
    (get-playing-time) ) )

(defun play(user grid)
  (let ((deadline (get-deadline user)))
    (set-user-feedback (user-feedback user))
    (gc) ;)
    (clear-screen)
    (print grid)
    (format t "Playing...~%")
    (if
      (or (winnerp grid) (loserp grid)) ; prevents a use-less first move by humans.
      (create-result grid)
      (time (user-play user grid deadline)) ) ) )

(defun play-again-p()
   "Returns T if the user wishes to play again"
   (y-or-n-p "~%Do you wish to play again?") )

(defun winnerp(grid)
   "Returns T iff the grid is solved."
   (null (compress (grid-board grid))) )

(defun loserp(grid)
   "Returns T iff the grid cannot be solved."
   (not
      (or (grid-has-combos (grid-board grid))
          (grid-has-combos
            (transpose (pad (grid-board grid) (grid-rows grid) (grid-cols grid) NIL)) ) ) ) )

(defun game-over(result)
  (print result)
  (result-winner result) )

(defun pause NIL (format t "~%Press <Enter> to continue...~%") (read-char) )

(defun directions()
  (clear-screen)

  (format t "Welcome to Chainshot!~%~%")
  (format t "Overview and objective:~%~%")
  (format t "The game consists of a board filled with beads. ")
  (format t "The objective of this game is to remove as many beads as possible - hopefully, every bead - from the grid.~%")
  
  (pause)

  (format t "How to play:~%~%")
  (format t "Beads of the same color that touch each other vertically or horizontally are considered a group. ")
  (format t "Any group can be removed from the board during a move, but single beads cannot be removed.~%")

  (pause)

  (format t "When a group is removed, all of the beads supported by the group drop down as far as possible in their respective columns. ")
  (format t "The dropping columns may break up old groups or form new groups in their new positions. ")
  (format t "If a column becomes empty, every column to its right shifts to the left to fill the empty column.~%")

  (pause)

  (format t "You will now be asked to choose a user (human or AI), and a board-type...~%")

  (pause)
  T )

(defun main()
  (game-over
    (play
      (prompt-for-user)
      (create-grid (prompt-for-grid-maker)) ) ) )

(defun chainshot()
  (when (y-or-n-p "Would you like to see the directions?") (directions))
  (main)
  (do NIL
    ((not (play-again-p)))
    (main) )
  (format t copyright)
  T )