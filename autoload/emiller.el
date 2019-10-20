;;; ~/.config/doom/autoload/emiller.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emiller/find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun +emiller/browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun +emiller/visit-i-org ()
  "Edit i.org"
  (interactive)
  (find-file "~/Dropbox/orgfiles/i.org"))

;;;###autoload
(defun +emiller/find-notes-for-major-mode ()
  "TODO"
  (interactive)
  (let ((default-directory (expand-file-name "code/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

;;;###autoload
(defun +emiller/find-notes-for-project ()
  "TODO"
  (interactive)
  (let ((default-directory (expand-file-name "projects/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (doom-project-name 'nocache) ".org"))))))
