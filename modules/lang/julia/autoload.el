;;; lang/julia/autoload.el -*- lexical-binding: t; -*-

;; `ob-julia' needs this variable to be defined, but it's defined in
;; `ess-custom', which won't be available if you're using :lang julia and not
;; :lang ess.
;;;###autoload (defvar inferior-julia-program-name (or (executable-find "julia-basic") "julia"))

;;;###autoload
(defun +julia/open-repl ()
  "Run an inferior instance of `julia' inside Emacs."
  (interactive)
  (julia-snail)
  (current-buffer))
