(defstruct (player
  (:constructor create-player (move-function play-function feedback name))
  (:print-function
    (lambda (player s k)
      (format s "~A" (player-name player)) ) ) )
  (name NIL :type simple-string)
  (feedback NIL)
  move-function
  play-function )

(defun create-human-player()
  (create-player 'human-move 'play-as-human T "Human"))

(defun next-move(player grid)
   (funcall (player-move-function player) grid) )

(defun player-play(player grid deadline)
  (funcall (player-play-function player) player grid deadline) )