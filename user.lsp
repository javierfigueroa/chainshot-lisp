(defstruct (user
  (:constructor create-user (move-function play-function feedback name))
  (:print-function
    (lambda (user s k)
      (format s "~A" (user-name user)) ) ) )
  (name NIL :type simple-string)
  (feedback NIL)
  move-function
  play-function )

(defun create-human-user()
  (create-user 'human-move 'play-as-human T "Human"))

(defun next-move(user grid)
   (funcall (user-move-function user) grid) )

(defun user-play(user grid deadline)
  (funcall (user-play-function user) user grid deadline) )