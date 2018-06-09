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
     :desc "Org-noter" :n "o" #'org-noter)))
 ;; (:after org
 ;; (:map org-mode-map
 ;;   :n "M-j" #'org-metadown
 ;;   :n "M-k" #'org-metaup))

(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
(remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;;
;; Modules
;;

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

;; Google Calendar
    (def-package! org-gcal
      :config
      (setq org-gcal-client-id "891288798426-io67fcj0069t6k9bldhm4b5mu0pbr0ph.apps.googleusercontent.com"
            org-gcal-client-secret "D6dedPByS6mzeCvg_FQyAEuk"
            org-gcal-file-alist '(("edmund.a.miller@gmail.com" .  "~/Dropbox/orgfiles/gcal.org") ;; Edmund Miller
                                  ("buvuk4b1vjghore8gsq6ifbcnk@group.calendar.google.com" .  "~/Dropbox/orgfiles/Lab_schedule.org") ;; Functional Genomics
                                  ("sgv1ng3qi5erm89f227h4hm02s@group.calendar.google.com" .  "~/Dropbox/orgfiles/schedule.org") ;; Org
                                  ))
    )

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
