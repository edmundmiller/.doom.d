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

;;;###autoload
(defun ediff-init-files ()
  (interactive)
  (ediff-files (expand-file-name "init.el" doom-private-dir)
               (expand-file-name "init.example.el" user-emacs-directory)))

;;;###autoload
(defun insert-todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))
(global-set-key (kbd "C-c d") 'insert-todays-date)
