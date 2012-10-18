(let ((all-files '(
"parse.lsp"
"utility.lsp"
"cs_grid.lsp"
"cs_turn.lsp"
"ai.lsp"
"users.lsp"
"settings.lsp"
"grid.lsp"
"grid-maker.lsp"
"visitor.lsp"
"result.lsp"
"main.lsp") ))

(defun reload()
  (loop for f in all-files do (load f))
  T )

(defun load-files()
  (compile-all)
  (load-compiled)
  (verbose-off)
  (gc)
  (clear-screen) )

(defun lsp-to-fas(name)
  (concatenate
    'simple-string
    (subseq name 0 (- (length name) 4))
    ".fas" ) )

(defun compile-all()
  (loop
    for file in all-files do
    (compile-file file)
    T ) )

(defun load-compiled()
  (loop
    for file in all-files do
    (load (lsp-to-fas file))
    T ) )

) ; end-let

(load-files)