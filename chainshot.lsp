;;File loader:

(let ((all-files '(
"defaults.lsp"
"utility.lsp"
"grid.lsp"
; "result.lsp"
"player.lsp"
"guest.lsp"
"dynamics.lsp"
"builder.lsp"
"dataset.lsp"
"main.lsp"
) ))

(defun load-files()
  (compile-all)
  (load-compiled)
  (turn-verbose-off)
  (gc)
  (clean-terminal) )

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

(defun lsp-to-fas(name)
  (concatenate
    'simple-string
    (subseq name 0 (- (length name) 4))
    ".fas" ) )

(defun reload-files()
  (loop for f in all-files do (load f))
  T )
) ; end-let

;; Load files

(load-files)
