;; Utility methods:

(defun turn-verbose-off()
  (setq *print-verbose* NIL) )

(defun turn-verbose-on()
  (setq *print-verbose* T) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun clean-terminal()
  "Clear the terminal window."
  #+WIN32 (run-shell-command "cls")
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun press-enter 
  NIL (format t "~%Press Enter to continue~%") (read-char) )

(defun get-deadline(player)
  (if (player-feedback player)
    60
    (get-playing-time) ) )

(defun get-playing-time()
  (format t "~%Thanks for asking me to play!~%About how long should I search? (in seconds)~%")
  (let 
    ((search-time-seconds
       (validate-input
        #'(lambda (choice)
            (and (realp choice) (plusp choice)) ) ) ))
    (+
      (now)
      (* search-time-seconds internal-time-units-per-second) ) ) )

(defun set-player-feedback(feedback)
  (setq *player-feedback* feedback) )

(defun feedback(&rest args)
  (if *player-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun validate-input(test &optional (message "Invalid input, try again...~%"))
   (do ((val (read) (read)))
       ((funcall test val) val)
     (format t message) ) )

(defun is-solved(grid)
   "T if the grid was solved."
   (null (compress (grid-board grid))) )

(defun is-not-solved(grid)
   "T if the grid is not solved."
   (not
      (or (grid-has-combos (grid-board grid))
          (grid-has-combos
            (transpose (pad (grid-board grid) (grid-rows grid) (grid-cols grid) NIL)) ) ) ) )

(defun transpose (x)
   "Returns a transpose of x (like a matrix).
    The row width of x' will be the minimum of the column lengths of x.
    The column length of x' will be the minimum of the row widths of x."
   (apply #'mapcar (cons #'list x)) )