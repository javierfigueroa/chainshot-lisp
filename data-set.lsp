
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
             ((or (string= "" line) (not line)))
            (setq strings (cons line strings)) )
         (grid-from-board (transpose (split-strings strings))) ) ) )

(defun safe-read-line(is)
   (read-line is NIL NIL) )
