;; Helper methods:

(defun clear-screen()
  "Tries to clear the screen/shell window where the current program is executing."
  #+WIN32 (run-shell-command "cls")
  #+UNIX (run-shell-command "clear") 
  NIL )

(defun int-to-color(color)
   (int-char (+ (char-code #\A) color)) )

(defun create-hash-table(&rest values)
  (let ((ht (make-hash-table))
        (index (list-length values)))
    (loop for val in (reverse values) do
      (setf (gethash index ht) val)
      (setq index (1- index)) )
    ht) )

(defun read-valid(test &optional (message "Please try again...~%"))
   (do ((val (read) (read)))
       ((funcall test val) val)
     (format t message) ) )

(defun between(num lower upper)
   (and (< lower num) (< num upper)) )

(defun get-positive-int(message)
  (read-valid
     #'(lambda (x)
        (and (integerp x) (> x 0)) )
   message ) )

(defun transpose (x)
   "Returns a transpose of x (like a matrix).
    The row width of x' will be the minimum of the column lengths of x.
    The column length of x' will be the minimum of the row widths of x."
   (apply #'mapcar (cons #'list x)) )

(defun verbose-off()
  (setq *print-verbose* NIL) )

(defun verbose-on()
  (setq *print-verbose* T) )

(defun verbose(&rest args)
  (if *print-verbose*
    (multiple-value-call 'format t (values-list args))
  NIL ) )

(defun set-user-feedback(feedback)
  (setq *user-feedback* feedback) )

(defun feedback(&rest args)
  (if *user-feedback*
    (multiple-value-call 'format t (values-list args))
  NIL ) )
