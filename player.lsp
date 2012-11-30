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

(defun next-move(player grid)
   (funcall (player-move-function player) grid) )

(defun player-play(player grid playing-time)
  (funcall (player-play-function player) player grid playing-time) )

(defun set-player-feedback(feedback)
  (setq *player-feedback* feedback) )

(defun feedback(&rest args)
  (if *player-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun get-time-limit()
    (format t "~%For how long should the AI play (seconds)?~%")
    (let 
      ((search-time-seconds
         (validate-input
          #'(lambda (choice)
              (and (realp choice) (plusp choice)) ) ) ))
      (+
        (now)
        (* search-time-seconds internal-time-units-per-second) ) ) )

(defun get-time(player)
  (if (player-feedback player)
    60
    (get-time-limit) ) )

(defun prompt-for-player()
  (let ((player-dictionary
    (create-dictionary
      (create-player 'human-move 'play-as-human T "Human")
      (create-player 'single-move 'first-come-first-played NIL "AI: The first come first play")
      (create-player 'single-move 'depth-first-search NIL "AI: depth-first search based play") ) ))
   (format t "~%Select a player type:~%")
   (maphash
     #'(lambda (key entry) (format t "  ~A : ~A~%" key entry))
     player-dictionary)
   (gethash
      (validate-input #'(lambda (choice) (and (numberp choice) (not (null (gethash choice player-dictionary))))))
      player-dictionary) ) )