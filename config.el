;;; config.el --- description -*- lexical-binding: t; -*-

(defvar xdg-data (getenv "XDG_DATA_HOME"))

(setq +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t)

      ;; mu4e-maildir        (expand-file-name "mail" xdg-data)
      ;; mu4e-attachment-dir (expand-file-name "attachments" mu4e-maildir))

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

;; Henrik's Magit split
(after! magit
    (setq magit-display-buffer-function #'+magit-display-buffer-fullscreen)
    (defun +magit-display-buffer-fullscreen (buffer)
    (display-buffer
    buffer (cond ((derived-mode-p 'magit-mode)
                    (when (eq major-mode 'magit-status-mode)
                    (display-buffer-in-side-window
                        (current-buffer) '((side . left) (window-width . 0.35))))
                    '(display-buffer-same-window))
                    ((bound-and-true-p git-commit-mode)
                    '(display-buffer-below-selected))
                    ((buffer-local-value 'git-commit-mode buffer)
                    '(magit--display-buffer-fullframe))
                    ((memq (buffer-local-value 'major-mode buffer)
                        '(magit-process-mode
                            magit-revision-mode
                            magit-log-mode
                            magit-diff-mode
                            magit-stash-mode))
                    '(display-buffer-in-side-window))
                    ('(magit--display-buffer-fullframe))))))
;;
;; Keybindings
;;

(map!
 (:leader
   (:prefix "o"
     :desc "Agenda" :n "a" #'org-agenda
     :desc "eShell" :n "e" #'+eshell/open-popup
     :desc "i.org" :n "o" #'emiller/visit-i-org
     :desc "APP: Email" :n "m" #'=mail)
   (:prefix "p"
     :desc "Counsel-ag" :n "f" #'counsel-ag)
   (:prefix "g"
     :desc "Git Status" :n "g" #'magit-status
     :desc "List gists" :n "l" #'+gist:list)
   (:prefix "n"
     :desc "Org-noter" :n "o" #'org-noter)
   (:prefix "p"
     :desc "Org-pomodoro" :n "s" #'org-pomodoro)
  :desc "New workspace" :n "N" (lambda! () (+workspace/new (read-string "Enter workspace name: ")))))
 ;; (:after org
 ;; (:map org-mode-map
 ;;   :n "M-j" #'org-metadown
 ;;   :n "M-k" #'org-metaup))

(after! org
  (setq org-directory "~/Dropbox/orgfiles")

  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename))
    (setq org-index-file (org-file-path "i.org"))
    (setq org-archive-location
        (concat (org-file-path "archive.org") "::* From %s"))

    (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
                                "~/Dropbox/orgfiles/i.org"
                                "~/Dropbox/orgfiles/Lab_Notebook.org"
                                "~/Dropbox/orgfiles/Lab_schedule.org"
                                "~/Dropbox/orgfiles/schedule.org"))

    ;; Set Bullets to OG
    (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
    (setq org-ellipsis " ▼ ")
    (setq org-export-with-toc nil)
    ;; Log when things are done
    (setq org-log-done 'time)

  (setq org-capture-templates
    '(("a" "Appointment" entry
       (file  "~/Dropbox/orgfiles/gcal.org" "Appointments")
       "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")

      ("n" "Note" entry
       (file+headline "~/Dropbox/orgfiles/i.org" "Notes")
       "** %?\n%T")

      ("l" "Link" entry
       (file+headline "~/Dropbox/orgfiles/links.org" "Links")
       "* %? %^L %^g \n%T" :prepend t)

      ("t" "To Do Item" entry
       (file+headline "~/Dropbox/orgfiles/i.org" "Unsorted")
       "*** TODO %?\n%T" :prepend t)

      ("j" "Lab Entry" entry
       (file+olp+datetree "~/Dropbox/orgfiles/Lab_Notebook.org" "Lab Journal")
       "** %? %^g \n\n")

      ("d" "Lab To Do" entry
       (file+headline "~/Dropbox/orgfiles/Lab_Notebook.org" "To Do")
       "** TODO %?\n%T" :prepend t)

      ("o" "Work To Do" entry
       (file+headline "~/Dropbox/orgfiles/o.org" "Unsorted")
       "** TODO %?\n%T" :prepend t))))

;;
;; Modules
;;

;; org-pomodoro
(def-package! org-pomodoro)
;; org-noter
(def-package! org-noter
  :config
  (map!
   (:leader
     (:prefix "n"
   :desc "Org-noter-insert" :n "i" #'org-noter-insert-note))))

;; solidity-mode
(set! solidity-solc-path "~/.node_modules/lib/node_modules/solc/solcjs")
(set! solidity-solium-path "~/.node_modules/lib/node_modules/solium/bin/solium.js")

;; ivy-yasnippet
(def-package! ivy-yasnippet
  :commands (ivy-yasnippet)
  :config
    (map!
     (:leader
       (:prefix "s"
         :desc "Ivy-yasnippet" :n "y" #'ivy-yasnippet))))

;; Dired all-the-icons
;; Shows the wrong faces
;; (def-package! all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; Docker
(def-package! docker)
;; Simple-mpc
(def-package! libmpdel)
(def-package! mpdel
  :after libmpdel
  :config
  (mpdel-mode))
;; Floobits
(def-package! floobits)

;; Edit Server
(def-package! edit-server
		:config
				(edit-server-start))

;; PDF-tools
(def-package! pdf-tools
  :preface
  (setq pdf-view-use-unicode-ligther nil)
  :config
  (map! (:map (pdf-view-mode-map)
          :n doom-leader-key nil))
  (set! :popup "\\*Outline " '((side . left) (size . 30)) '((quit . t)))
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-midnight-colors `(,(doom-color 'fg) . ,(doom-color 'bg)))
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))

;; emms
;; (use-package emms
;;   :ensure t
;;   :config
;;     (require 'emms-setup)
;;     (require 'emms-player-mpd)
;;     (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
;;     (setq emms-seek-seconds 5)
;;     (setq emms-player-list '(emms-player-mpd))
;;     (setq emms-info-functions '(emms-info-mpd))
;;     (setq emms-player-mpd-server-name "localhost")
;;     (setq emms-player-mpd-server-port "6601")
;;     (setq mpc-host "localhost:6601"))

(provide 'config)
;;; config.el ends here
