;; This file contains utility functions used across the game

;;Some defaults
(setq *print-verbose* NIL)
(setq *player-feedback* T)

(defun clear-terminal()
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun validate-input(test &optional (message "Invalid input, try again.~%"))
   (do ((val (read) (read)))
       ((funcall test val) val)
     (format t message) ) )

(defun is-between(num lower upper)
   (and (< lower num) (< num upper)) )

(defun transpose (x)
   "Do a transpose of x."
   (apply #'mapcar (cons #'list x)) )

(defun set-verbose(verbose)
  (setq *print-verbose* verbose) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )