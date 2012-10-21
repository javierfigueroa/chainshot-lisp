;; This file contains the grid struct and the grid builder struct. Both with functions to handle grid related logic

(defstruct
  (grid (:print-function  print-grid))
  (board '() :type list)
  (colors 5 :type integer :read-only T) ;; default to 5 colors
  (rows 5 :type integer :read-only T) ;; default to 5 rows
  (cols 5 :type integer :read-only T) ;; default to 5 cols
  (print-function 'print-grid) )

(defun has-groups(grid)
 "Check of the grid has bead groups of the same same color"
  (loop for x from 1 to (get-grid-dimensions grid) do NIL
   when (not (null (loop for y from 1 to (get-grid-dimensions grid) do NIL
      when (>= (length (find-beads grid x y)) 3)
        return T )))
   return T) 
)

(defun get-grid-dimensions(grid) 
   "Get grid dimensions."
   (values (grid-rows grid) (grid-cols grid)) )

(defun show-grid(grid &optional (s t))
   "Print grid."
   (let* ((length (grid-cols grid)) (rows (grid-rows grid)))
      (show-grid-column
        (transpose
          (mapcar 'reverse
            (add-padding (clean-board (grid-board grid)) rows length  ".") ) )
         s (grid-cols grid))
      (format s "~%    ")
      (loop for x from 0 below rows do (format s "- -"))
      (format s "~%    ")
      (loop for x from 1 below (min 10 (1+ rows)) do (format s "  ~A" x))
      (loop for x from 10 below (1+ rows) do (format s " ~A" x)) ) )

(defun show-grid-column(board s index)
   "Print columns."
   (when (not (atom board))
      (show-grid-row (car board) s index)
      (show-grid-column (cdr board) s (1- index))
      T ) )

(defun show-grid-row(line s index)
   "Print row."
   (if (<= index 9)
       (format s "~% ~A |" index)
       (format s "~%~A |" index))
   (loop for x in line do
      (if (or (equalp "." x) (null x))
         (format s "  .")
         (format s "  ~A" x) ) ) )

(defun grid-beads-left-count(grid)
  "Returns the number of cells left in grid."
  (reduce '+
    (mapcar
      (lambda (line) (list-length (remove NIL line)))
      (grid-board grid) ) ) )

(defun print-grid(g s k)
  "Print the grid and footer info."
  (show-grid g s)
  (format s "~% ~D x ~D grid with ~D colors and ~D beads remaining~%"
    (grid-cols g)
    (grid-rows g)
    (grid-colors g)
    (grid-beads-left-count g) ) )

(defun copy-grid(grid)
  "Makes a copy of the grid."
  (make-grid
     :cols   (grid-cols grid)
     :rows   (grid-rows grid)
     :colors (grid-colors grid)
     :board   (mapcar 'copy-list (grid-board grid))
     :print-function (grid-print-function grid) ) )

(defun get-cell-color-for-grid(grid row col) 
   "Get color of the cell by grid."
   (get-cell-color-for-board (grid-board grid) row col) )

(defun get-cell-color-for-board(board row col) 
   "Get color of the cell by board."
  (if (and (plusp row) (plusp col))
    (nth (1- row) (nth (1- col) board))
    NIL ) )

(defun set-cell-color(grid row col val) 
   "Set color for grid cell."
   (setf (nth (1- row) (nth (1- col) (grid-board grid))) val) )

(defun add-padding(board rows length char) 
   "Adds NIL padding to the board."
   (cond ((<= rows 0) NIL)
         ((append (list (add-row-padding (car board) length char))
                  (add-padding (cdr board) (1- rows) length char))) ) )

(defun add-row-padding(row length char) 
   "Adds NIL padding to the row."
   (cond ((zerop length) NIL)
         ((null row) (cons char (add-row-padding '() (1- length) char)))
         ((cons (car row) (add-row-padding (cdr row) (1- length) char))) ) )

(defun clean-grid(grid) 
  "Removes cells with with NIL in the grid."
  (setf
    (grid-board grid)
    (add-padding
      (clean-board (grid-board grid))
      (grid-rows grid)
      (grid-cols grid)
      NIL ) )
  grid )

(defun clean-board(board) 
   "Removes cells with NIL in the board."
   (cond ((null board) '())
       ((null (car board)) (clean-board (cdr board)))
       ((let ((a (clean-line (car board)))
              (b (clean-board (cdr board))))
           (if (null a)
                b
                (cons a b))) ) ) )

(defun clean-line(line) ;; from top
   "Removes cells with NIL in the line."
   (cond ((null line) NIL)
      ((null (car line)) (clean-line (cdr line)))
      ((cons (car line) (clean-line (cdr line)))) ) )

;; Grid builder - Used to construct a grid object and other grid helper methods

(defstruct (grid-builder
   (:constructor create-grid-builder (name function))
   (:print-function
       (lambda (grid-builder s k)
           (format s "~A" (grid-builder-name grid-builder)) ) ) )
   (name NIL :type simple-string)
   (function 'read-grid-from-path) )


(defun read-grid-from-path()
 (create-grid-builder "File" 'grid-from-path))

(defun grid-from-path(grid-builder)
   (format t "~%Enter the name of the grid file:~%")
   (parse-input
     (validate-input
      #'(lambda (choice) (probe-file choice))
      "File does not exist~%" ) ) )

(defun grid-from-board(board size)
      (make-grid :rows size :cols size :board board :colors (count-distinct-beads board)) )

(defun create-grid(grid-builder)
   (funcall (grid-builder-function grid-builder) grid-builder) )

(defun count-distinct-beads(some-list)
   (list-length
      (remove-duplicates
         (remove NIL
            (reduce 'append some-list) ) ) ) )

