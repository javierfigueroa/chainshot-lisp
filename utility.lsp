;; Utility methods:


(defun set-verbose-off()
  (setq *print-verbose* NIL) )

(defun set-verbose-on()
  (setq *print-verbose* T) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun set-player-feedback(feedback)
  (setq *player-feedback* feedback) )

(defun player-feedback(&rest args)
  (if *player-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun clean-terminal()
  "Clear the terminal window."
  #+WIN32 (run-shell-command "cls")
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun press-enter 
  NIL (format t "~%Press Enter to continue~%") (read-char) )

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

(defun create-map(&rest values)
(let ((map (make-hash-table))
      (index (list-length values)))
  (loop for val in (reverse values) do
    (setf (gethash index map) val)
    (setq index (1- index)) )
  map) )

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
