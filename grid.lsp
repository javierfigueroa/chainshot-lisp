(defstruct
  (grid (:print-function  print-grid))
  (board '() :type list)
  (colors default-colors :type integer :read-only T)
  (rows default-w :type integer :read-only T)
  (cols default-len :type integer :read-only T)
  (print-function default-print-function) )

(defun grid-has-combinations(grid) ;from main
   "Checks the grid to see if it has any vertical combinations available."
   (reduce #'(lambda (x y) (or x y)) (mapcar 'has-triple grid)) )
; 
(defun has-pairs(line &optional (length (list-length line)))
   "Check if the line has two of any non-null color beads."
   (cond
      ((or (zerop length) (null (car line)) (null (cdr line))) NIL)
      ((equalp (nth 0 line) (nth 1 line)) T)
      ((has-pairs (cdr line) (1- length))) ) )

(defun has-triple(line &optional (length (list-length line)))
   "Check if the line has two of any non-null color beads."
   (cond
      ((or (zerop length) (<= length 2) (null (car line)) (null (cdr line))) NIL)
      ((and  (equalp (nth 0 line) (nth 1 line)) (equalp (nth 1 line) (nth 2 line))) T)
      ((has-triple (cdr line) (1- length))) ) )

(defun get-grid-dimensions(grid) 
   "Get grid dimensions."
   (values (grid-rows grid) (grid-cols grid)) )

(defun show-grid(grid &optional (s t))
   "Print grid."
   (let* ((length (grid-cols grid)) (rows (grid-rows grid)))
      (show-grid-inner
        (transpose
          (mapcar 'reverse
            (add-padding (clean-board (grid-board grid)) rows length  ".") ) )
         s (grid-cols grid))
      (format s "~%    ")
      (loop for x from 0 below rows do (format s "- -"))
      (format s "~%    ")
      (loop for x from 1 below (min 10 (1+ rows)) do (format s "  ~A" x))
      (loop for x from 10 below (1+ rows) do (format s " ~A" x)) ) )

(defun show-grid-inner(board s index)
   "Print columns."
   (when (not (atom board))
      (show-line (car board) s index)
      (show-grid-inner (cdr board) s (1- index))
      T ) )

(defun show-line(line s index)
   "Print row."
   (if (<= index 9)
       (format s "~% ~A |" index)
       (format s "~%~A |" index))
   (loop for x in line do
      (if (or (equalp "." x) (null x))
         (format s "  .")
         (format s "  ~A" x) ) ) )


(defun grid-count(grid)
  "Returns the number of cells left in grid."
  (reduce '+
    (mapcar
      (lambda (line) (list-length (remove NIL line)))
      (grid-board grid) ) ) )

(defun print-grid(g s k)
  (show-grid g s)
  (format s "~% ~D x ~D grid with ~D colors and ~D beads remaining~%"
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

;; Grid builder

(defstruct (grid-builder
   (:constructor create-grid-builder (name function))
   (:print-function
       (lambda (grid-builder s k)
           (format s "~A" (grid-builder-name grid-builder)) ) ) )
   (name NIL :type simple-string)
   (function 'read-grid-from-path) )


(defun split-strings(strings)
   (loop for string in strings
      collect
      (loop for i from 0 below (array-dimension string 0) 
         collect
         (aref string i) ) ) )

(defun read-grid-from-path()
 (create-grid-builder "File" 'grid-from-path))

(defun grid-from-path(grid-builder)
   (format t "~%Enter the name of the grid file:~%")
   (parse-data-set
     (validate-input
      #'(lambda (choice) (probe-file choice))
      "File does not exist~%" ) ) )

(defun grid-from-board(board)
   (let ((width (list-length board))
         (length (list-length (car board))) )
      (make-grid :rows width :cols length :board board :colors (count-distinct-beads board)) ) )

(defun create-grid(grid-builder)
   (funcall (grid-builder-function grid-builder) grid-builder) )

(defun count-distinct-beads(some-list)
   (list-length
      (remove-duplicates
         (remove NIL
            (reduce 'append some-list) ) ) ) )

