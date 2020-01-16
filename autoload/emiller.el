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
(defun +emiller/visit-todo-org ()
  "Edit todo.org"
  (interactive)
  (find-file
    (expand-file-name +org-capture-todo-file org-directory)))

;;;###autoload
(defun +emiller/visit-projects-org ()
  "Edit todo.org"
  (interactive)
  (find-file
    (expand-file-name +org-capture-projects-file org-directory)))
