(defstruct (data-set
   (:include grid-builder)
   (:print-function
       (lambda (data-set s k)
           (format s "~A ~A" (grid-builder-name data-set) (data-set-desc data-set)) ) ) )
   (desc NIL :type simple-string)
   (path NIL :type simple-string) )

(defun create-data-set(name path desc)
   (make-data-set
      :name name
      :path path
      :desc desc
      :function #'(lambda (data-set) (parse-data-set (data-set-path data-set))) ) )

(defun parse-data-set(data_set)		
   (with-open-file (is data_set :direction :input
                    :if-does-not-exist nil)
      (let ((strings NIL))
         (do ((line (safe-read-line is) (safe-read-line is)))	
             ((or (<= (length line) 1)(string= "" line) (not line)))
            (setq strings (cons (join-string-list (split-by-one-space line)) strings)) )
     
				format t "debug ~A" strings
     (if ( and (not (null strings))(> (length strings) 0))
         (grid-from-board (transpose (split-strings strings)))) ) ) )

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