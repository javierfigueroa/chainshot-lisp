(setq default-width 5)
(setq default-length 5)
(setq default-colors 5)
(setq default-print-function 'report-grid-pretty)

(setq *print-verbose* NIL)
(setq *user-feedback* T)


(let ((all-files '(
"utility.lsp"
; "helpers.lsp"
; "ai.lsp"
"dynamics.lsp"
"user.lsp"
; "settings.lsp"
"grid.lsp"
"grid-builder.lsp"
"visitor.lsp"
"result.lsp"
"main.lsp") ))

(defun reload()
  (loop for f in all-files do (load f))
  T )

(defun load-files()
  (clean-files)
  (compile-all)
  (load-compiled)
  (verbose-off)
  (gc)
  (clear-screen) )

(defun clean-files()
  #+UNIX (run-shell-command "rm *.fas *.lib") 
  NIL)

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