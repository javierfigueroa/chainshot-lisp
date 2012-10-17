;; Utility methods:

(defun clean-terminal()
  "Clear the terminal window."
  #+WIN32 (run-shell-command "cls")
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun press-enter NIL (format t "~%Press Enter to continue~%") (read-char) )

(defun turn-verbose-off()
  (setq *print-verbose* NIL) )

(defun turn-verbose-on()
  (setq *print-verbose* T) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )