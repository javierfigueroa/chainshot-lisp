(defstruct (user
  (:constructor create-user (move-function play-function feedback name))
  (:print-function
    (lambda (user s k)
      (format s "~A" (user-name user)) ) ) )
  (name NIL :type simple-string)
  (feedback NIL)
  move-function
  play-function )

;; User prompts...

(defun prompt-for-user()
  (create-user 'human-move 'user-friendly T "Human"))

;; Does a call-back to user's move- and play- functions.

(defun next-move(user grid)
   (funcall (user-move-function user) grid) )

(defun user-play(user grid deadline)
  (funcall (user-play-function user) user grid deadline) )