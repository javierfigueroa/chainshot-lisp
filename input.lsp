(defstruct (input
   (:include grid-builder)
   (:print-function
       (lambda (input s k)
           (format s "~A ~A" (grid-builder-name input) (input-desc input)) ) ) )
   (desc NIL :type simple-string)
   (path NIL :type simple-string) )

(defun create-input(name path desc)
   (make-input
      :name name
      :path path
      :desc desc
      :function #'(lambda (input) (parse-input (input-path input))) ) )

(defun parse-input(file)
   "Read file, assumes first line is the size of the grid and following lines a row of the grid"
   (with-open-file (is file :direction :input
                    :if-does-not-exist nil)
      ;; Set the size of the grid
      (setq size (parse-integer (safe-read-line is))) 
 
      (let ((strings NIL))
         (do ((line (safe-read-line is) (safe-read-line is)))
             ((or (string= "" line) (not line)))
            ;; Remove spaces from strings and assign to strings variable looks like  ((b b g r r) (b b g r l)...
            (setq strings (cons (join-string-list (split-by-one-space line)) strings)) )
         (grid-from-board (transpose (split-strings strings)) size) ) ) )

(defun safe-read-line(is)
   (read-line is NIL NIL) )

(defun split-strings(strings)	
  (loop for string in strings	do
      collect
      (loop for i from 0 below (array-dimension string 0) do
         collect
         (aref string i) ) ) )

(defun split-by-one-space (string)
    "Split a string by 'space'."
	
    (loop for i = 0 then (1+ j) do
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun join-string-list (string-list)
    "Concatenates a list of strings."
    (format nil "~{~A~^~}" string-list))