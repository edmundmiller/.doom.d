;;; ~/.config/doom/autoload/emiller.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emiller/find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config/dotfiles")))

;;;###autoload
(defun +emiller/browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.config/dotfiles")))

;;;###autoload
(defun +emiller/visit-inbox-org ()
  "Edit inbox.org"
  (interactive)
  (find-file
   (expand-file-name "roam/project/inbox.org" org-directory)))

;;;###autoload
(defun +emiller/visit-projects ()
  "Visit roam projects"
  (interactive)
  (find-file
   (expand-file-name "roam/project/" org-directory)))

;;;###autoload
(defun ediff-init-files ()
  (interactive)
  (ediff-files (expand-file-name "init.el" doom-private-dir)
               (expand-file-name "init.example.el" user-emacs-directory)))

;;;###autoload
(defun +emiller/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  (org-map-entries 'org-archive-subtree "/[X]" 'file)
  (org-map-entries 'org-archive-subtree "/KILL" 'file))

;;;###autoload
(defun modi/lower-case-org-keywords ()
  "Lower case Org keywords and block identifiers.

Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

Inspiration:
https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
      ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
      ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

;;;###autoload
(defun elfeed-v-mpv (url)
  "Watch a video from URL in MPV"
  (async-shell-command (format "mpv '%s'" url)))

;;;###autoload
(defun elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (elfeed-v-mpv it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))
