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

(defun press-enter NIL (format t "~%Press Enter to continue...~%") (read-char) )

(defun rules()
  (clear-screen)

  (format t "....==================....~%~%")
  (format t "Your objective:~%~%")
  (format t "....==================....~%~%")
  (format t "You'll be presented with a grid of 'beads' represented by G for green, ")
  (format t "B for blue, O for orange, R for red and L for black. ")
  (format t "Your objective is to clear as many beads of the grid in the least amount of steps as possible.~%~%")
  (format t "....==================....~%~%")
  (format t "Your moves:~%~%")
  (format t "....==================....~%~%")
  (format t "You'll need to choose a column and row from the grid to provide the location of a bead within the grid. The beads of the same color that are adjacent to each other, either vertically or horizontally are candidates for removal.")
  (format t "Only beads that are groups that consist of 3 beads or more will be removed upon a move.~%~%")
  (format t "....==================....~%~%")
  (format t "Some dynamics of the game:~%~%")
  (format t "....==================....~%~%")
  (format t "Upon removal of a group of beads, the empty spots are filled with beads on higher positions, a 'gravity' effect is applied to the affected columns. If an entire column becomes empty then the next column is pushed to the left to fill in the gap.~%~%")
  (press-enter)
  T )
 

(defun main()
  (game-over
    (play
      (prompt-for-user)
      (create-grid (prompt-for-grid-maker)) ) ) )

(defun chainshot()	  
  (when (y-or-n-p "Welcome to Chainshot! Review rules of the game?") (rules))
  (main)
  (do NIL
    ((not (play-again-p)))
    (main) )
  T )