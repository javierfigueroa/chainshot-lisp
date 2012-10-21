;; This file contains the player struct and function related to the player's logic

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

(defun set-player-feedback(feedback)
  (setq *player-feedback* feedback) )

(defun feedback(&rest args)
  (if *player-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun get-time(player)
  (if (player-feedback player)
    60
    (get-playing-time) ) )