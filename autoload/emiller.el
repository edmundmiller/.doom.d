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
  (find-file +org-capture-todo-file))

;;;###autoload
(defun +emiller/visit-projects ()
  "Visit roam projects"
  (interactive)
  (find-file
   (expand-file-name "roam/project/" org-directory)))

;;;###autoload
(defun +emiller/show-agenda ()
  "Toggle org-agenda frame. If frame exists and is focused, close it.
If frame exists but isn't focused, bring it to front.
If frame doesn't exist, create a new one."
  (let ((agenda-frame (+emiller/find-agenda-frame)))
    (cond
     ;; If agenda frame exists and is currently selected, close it
     ((and agenda-frame (eq agenda-frame (selected-frame)))
      (delete-frame agenda-frame))
     ;; If agenda frame exists but isn't selected, focus it and refresh agenda
     (agenda-frame
      (select-frame agenda-frame)
      (x-focus-frame agenda-frame)
      ;; Refresh the agenda to make sure it's current
      (when (get-buffer "*Org Agenda*")
        (with-current-buffer "*Org Agenda*"
          (org-agenda-redo))))
     ;; If no agenda frame exists, create a new one
     (t
      (let ((new-frame (make-frame-command)))
        (select-frame new-frame)
        ;; Set a parameter to identify this as the agenda frame
        (set-frame-parameter new-frame 'agenda-frame t)
        ;; Set frame title for easy identification
        (set-frame-parameter new-frame 'title "Org Agenda")
        (org-agenda-list)
        (x-focus-frame new-frame))))))

;;;###autoload
(defun +emiller/find-agenda-frame ()
  "Find the frame designated as the agenda frame."
  (catch 'found
    (dolist (frame (frame-list))
      (when (frame-parameter frame 'agenda-frame)
        (throw 'found frame)))
    nil))

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
