(defun chainshot()	  
  (when (y-or-n-p "Welcome to Chainshot! Review rules of the game?") (rules))
  (main)
  (do NIL
    ((not (play-again)))
    (main) )
  T )

(defun main()
  (game-over
    (play
      (create-human-player)
      (create-grid (read-grid-from-path)) ) ) )


(defun play(player grid)
  (let 
	((deadline (get-time player)))
    (set-player-feedback (player-feedback player))
    (gc) ;)
    (clear-screen)
    (print grid)
    (format t "Playing with following board:~%")
    (if
      (or (is-solved grid) (is-not-solved grid)) ; prevents a use-less first move by humans.
      (create-output grid)
      (time (player-play player grid deadline)) ) ) )

(defun play-again()
   (y-or-n-p "~%Play again?") )

(defun is-solved(grid)
   (null (clean-board (grid-board grid))) )

(defun is-not-solved(grid)
   (not
      (or (grid-has-combinations (grid-board grid))
          (grid-has-combinations
            (transpose (add-padding (grid-board grid) (grid-rows grid) (grid-cols grid) NIL)) ) ) ) )

(defun game-over(output)
  (print output)
  (output-winner output) )

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