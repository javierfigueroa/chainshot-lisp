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
  (let ((user-ht
    (create-hash-table
      (create-user 'human-move 'user-friendly T "Human: you!")
      (create-user 'single-move-per-combo 'first-combo-first-searched NIL "AI simple: The first combo it can find")
      (create-user 'single-move-per-combo 'depth-first NIL "AI good: depth-first search, strictly one move per combo") ) ))
   (format t "~%Please choose a user:~%")
   (maphash
     #'(lambda (key entry) (format t "  ~A : ~A~%" key entry))
     user-ht)
   (gethash
      (read-valid #'(lambda (choice) (and (numberp choice) (not (null (gethash choice user-ht))))))
      user-ht) ) )

;; Does a call-back to user's move- and play- functions.

(defun next-move(user grid)
   (funcall (user-move-function user) grid) )

(defun user-play(user grid deadline)
  (funcall (user-play-function user) user grid deadline) )