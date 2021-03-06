;; This file contains utility functions used across the game

;;Some globals
(setq *logger* NIL)
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

(defun set-logger(log)
  (setq *logger* log) )

(defun logger(&rest args)
  (if *logger*
    (multiple-value-call 'format t (values-list args))
  NIL ) )
  
(defun create-dictionary(&rest values)
  (let ((hashtable (make-hash-table))
        (index (list-length values)))
    (loop for val in (reverse values) do
      (setf (gethash index hashtable) val)
      (setq index (1- index)) )
    hashtable) )
