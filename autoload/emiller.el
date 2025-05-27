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
(defun +emiller/org-calc-urgency ()
  "Compute a Taskwarrior-style urgency score for the current Org heading."
  (let* ((deadline (org-entry-get nil "DEADLINE"))
         (scheduled (org-entry-get nil "SCHEDULED"))
         (tags (org-get-tags))
         (prio (org-entry-get nil "PRIORITY"))
         (today (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))
         (score 0.0))
    ;; +15 for :next: tag
    (when (member "next" tags)
      (setq score (+ score 15.0)))
    ;; Deadline factor
    (when deadline
      (let* ((due-day (org-time-string-to-absolute (substring deadline 1 -1)))
             (days (- due-day today))
             (factor (cond ((>= days 14) 0.2)
                           ((>= days -7) (+ 0.2 (* 0.8 (/ (+ days 14.0) 21.0))))
                           (t 1.0))))
        (setq score (+ score (* 12.0 factor)))))
    ;; Scheduled tasks
    (when scheduled
      (setq score (+ score 5.0)))
    ;; Waiting TODO state
    (when (string= (org-get-todo-state) "WAITING")
      (setq score (- score 3.0)))
    ;; Priority A/B/C
    (when prio
      (setq score (+ score
                     (cond ((string= prio "A") 6.0)
                           ((string= prio "B") 3.9)
                           ((string= prio "C") 1.8)
                           (t 0.0)))))
    ;; Tags count factor
    (let ((n (length tags)))
      (when (> n 0)
        (setq score (+ score (* 1.0
                                (cond ((= n 1) 0.8)
                                      ((= n 2) 0.9)
                                      (t 1.0)))))))
    score))

;;;###autoload
(defun +emiller/org-urgency-cmp (a b)
  "Compare two agenda entries by custom urgency."
  (let* ((am (get-text-property 0 'org-marker a))
         (bm (get-text-property 0 'org-marker b))
         ;; Compute each entry's urgency by moving to its marker
         (ua (with-current-buffer (marker-buffer am)
               (org-with-point-at am (+emiller/org-calc-urgency))))
         (ub (with-current-buffer (marker-buffer bm)
               (org-with-point-at bm (+emiller/org-calc-urgency)))))
    ;; Sort descending (highest urgency first)
    (cond ((> ua ub) -1) ((< ua ub) 1) (t nil))))

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
