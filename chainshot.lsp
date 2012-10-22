;;This file contains the main functions that compile, load and start the game 

(let ((files '(
"utility.lsp"
"dynamics.lsp"
"player.lsp"
"grid.lsp"
"input.lsp"
"dead.lsp"
"output.lsp"
"main.lsp") ))

(defun load-files()
  (clean-files)
  (compile-all)
  (load-compiled)
  (set-logger NIL)
  (gc)
  (clear-terminal) )

(defun clean-files()
  "Clean previously compiled files."
  #+UNIX (run-shell-command "rm *.fas *.lib") 
  NIL)

(defun compile-all()
  "Compile files."
  (loop
    for file in files do
    (compile-file file)
    T ) )

(defun load-compiled()
  "Load compiled files."
  (loop
    for file in files do
    (load (lsp-to-fas file))
    T ) )

(defun lsp-to-fas(name)
  (concatenate
    'simple-string
    (subseq name 0 (- (length name) 4))
    ".fas" ) )
) 

;;Load files
(load-files)
;;Start the game
(chainshot)