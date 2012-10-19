(defstruct
  (grid (:print-function  report-grid))
  (board '() :type list)
  (colors default-colors :type integer :read-only T)
  (rows default-w :type integer :read-only T)
  (cols default-len :type integer :read-only T)
  (print-function default-print-function) )

;;; start checked

(defun grid-has-combinations(grid) ;from main
   "Checks the grid to see if it has any vertical combinations available."
   (reduce #'(lambda (x y) (or x y)) (mapcar 'has-pairs grid)) )

(defun has-pairs(line &optional (length (list-length line)))
   "Check if the line has two of any non-null color beads."
   (cond
      ((or (zerop length) (null (car line)) (null (cdr line))) NIL)
      ((equalp (nth 0 line) (nth 1 line)) T)
      ((has-pairs (cdr line) (1- length))) ) )

(defun get-grid-dimensions(grid) ;from visitor
   "Get grid dimensions."
   (values (grid-rows grid) (grid-cols grid)) )

;;; end checked

(defun report-grid(g s k)
   (funcall (grid-print-function g) g s k) )

(defun report-grid-count(g s k)
   (format s "~D remaining" (grid-count g)) )

(defun report-grid-debug(g s k)
   (format s "~D x ~D grid (~D colors)~%~A"
     (grid-cols g)
     (grid-rows g)
     (grid-colors g)
     (grid-board g) ) )

(defun report-grid-pretty(g s k)
  (show-grid g s)
  (format s "~% ~D x ~D grid (~D colors) ~~ ~D beads remain~%"
    (grid-cols g)
    (grid-rows g)
    (grid-colors g)
    (grid-count g) ) )

(defun copy-grid(grid)
  (make-grid
     :cols   (grid-cols grid)
     :rows   (grid-rows grid)
     :colors (grid-colors grid)
     :board   (mapcar 'copy-list (grid-board grid))
     :print-function (grid-print-function grid) ) )

(defun transpose-grid(grid)
  (let ((copy (copy-grid grid))
        (board (grid-board grid)) )
    (setf (grid-board copy) (transpose board))
    copy ) )