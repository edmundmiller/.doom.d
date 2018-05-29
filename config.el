;;; config.el --- description -*- lexical-binding: t; -*-

(defvar xdg-data (getenv "XDG_DATA_HOME"))

(setq +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t)

      ;; mu4e-maildir        (expand-file-name "mail" xdg-data)
      ;; mu4e-attachment-dir (expand-file-name "attachments" mu4e-maildir))

(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

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
(setq solidity-solc-path "~/.node_modules/lib/node_modules/solc/solcjs")
(setq solidity-solium-path "~/.node_modules/lib/node_modules/solium/bin/solium.js")

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

;; Company-box
(setq
   company-box-backends-colors nil
   company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
   company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
   company-box-icons-elisp
   (list (all-the-icons-material "functions" :height 0.8 :face 'all-the-icons-red)
         (all-the-icons-material "check_circle" :height 0.8 :face 'all-the-icons-blue)
         (all-the-icons-material "stars" :height 0.8 :face 'all-the-icons-orange)
         (all-the-icons-material "format_paint" :height 0.8 :face 'all-the-icons-pink)))
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

;; app/email
(after! mu4e
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-bookmarks
        `(("\\\\Inbox" "Inbox" ?i)
          ("\\\\Draft" "Drafts" ?d)
          ("flag:unread AND \\\\Inbox" "Unread messages" ?u)
          ("flag:flagged" "Starred messages" ?s)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)))

  (set! :email "gmail.com"
    '((mu4e-sent-folder       . "/gmail/Sent Mail")
      (mu4e-drafts-folder     . "/gmail/Drafts")
      (mu4e-trash-folder      . "/gmail/Trash")
      (mu4e-refile-folder     . "/gmail/All Mail")
      (smtpmail-smtp-user     . "edmund.a.miller")
      (user-mail-address      . "edmund.a.miller@gmail.com")))

  ;; an evil-esque keybinding scheme for mu4e
  (setq mu4e-view-mode-map (make-sparse-keymap)
        ;; mu4e-compose-mode-map (make-sparse-keymap)
        mu4e-headers-mode-map (make-sparse-keymap)
        mu4e-main-mode-map (make-sparse-keymap))

  (map! (:map (mu4e-main-mode-map mu4e-view-mode-map)
          :leader
          :n "," #'mu4e-context-switch
          :n "." #'mu4e-headers-search-bookmark
          :n ">" #'mu4e-headers-search-bookmark-edit
          :n "/" #'mu4e~headers-jump-to-maildir)

        (:map (mu4e-headers-mode-map mu4e-view-mode-map)
          :localleader
          :n "f" #'mu4e-compose-forward
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-new
          :n "e" #'mu4e-compose-edit)

        (:map mu4e-main-mode-map
          :n "q"   #'mu4e-quit
          :n "u"   #'mu4e-update-index
          :n "U"   #'mu4e-update-mail-and-index
          :n "J"   #'mu4e~headers-jump-to-maildir
          :n "c"   #'+email/compose
          :n "b"   #'mu4e-headers-search-bookmark)

        (:map mu4e-headers-mode-map
          :n "q"   #'mu4e~headers-quit-buffer
          :n "r"   #'mu4e-compose-reply
          :n "c"   #'mu4e-compose-edit
          :n "s"   #'mu4e-headers-search-edit
          :n "S"   #'mu4e-headers-search-narrow
          :n "RET" #'mu4e-headers-view-message
          :n "u"   #'mu4e-headers-mark-for-unmark
          :n "U"   #'mu4e-mark-unmark-all
          :n "v"   #'evil-visual-line
          :nv "d"  #'+email/mark
          :nv "="  #'+email/mark
          :nv "-"  #'+email/mark
          :nv "+"  #'+email/mark
          :nv "!"  #'+email/mark
          :nv "?"  #'+email/mark
          :nv "r"  #'+email/mark
          :nv "m"  #'+email/mark
          :n "x"   #'mu4e-mark-execute-all

          :n "]]"  #'mu4e-headers-next-unread
          :n "[["  #'mu4e-headers-prev-unread

          (:localleader
            :n "s" 'mu4e-headers-change-sorting
            :n "t" 'mu4e-headers-toggle-threading
            :n "r" 'mu4e-headers-toggle-include-related

            :n "%" #'mu4e-headers-mark-pattern
            :n "t" #'mu4e-headers-mark-subthread
            :n "T" #'mu4e-headers-mark-thread))

        (:map mu4e-view-mode-map
          :n "q" #'mu4e~view-quit-buffer
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-edit
          :n "o" #'ace-link-mu4e

          :n "<M-Left>"  #'mu4e-view-headers-prev
          :n "<M-Right>" #'mu4e-view-headers-next
          :n "[m" #'mu4e-view-headers-prev
          :n "]m" #'mu4e-view-headers-next
          :n "[u" #'mu4e-view-headers-prev-unread
          :n "]u" #'mu4e-view-headers-next-unread

          (:localleader
            :n "%" #'mu4e-view-mark-pattern
            :n "t" #'mu4e-view-mark-subthread
            :n "T" #'mu4e-view-mark-thread

            :n "d" #'mu4e-view-mark-for-trash
            :n "r" #'mu4e-view-mark-for-refile
            :n "m" #'mu4e-view-mark-for-move))

        (:map mu4e~update-mail-mode-map
            :n "q" #'mu4e-interrupt-update-mail))

  ;; EMiller Below this line. User Beware
  (setq mu4e-sent-folder "/home/emiller/.mail/gmail/sent"
        ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
        mu4e-drafts-folder "/home/emiller/.mail/gmail/drafts"
        user-mail-address "edmund.a.miller@gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  ;; Now I set a list of
  (defvar my-mu4e-account-alist
    '(("Gmail"
       (mu4e-sent-folder "/home/emiller/.mail/gmail/sent")
       (mu4e-drafts-folder "/gmail/Drafts")
       (user-mail-address "edmund.a.miller@gmail.com")
       (smtpmail-smtp-user "Edmund.a.Miller")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       )
      ("oldGmail"
       (mu4e-sent-folder "/home/emiller/.mail/oldGmail/sent")
       (mu4e-drafts-folder "/oldGmail/Drafts")
       (user-mail-address "eman0088@gmail.com")
       (smtpmail-smtp-user "eman0088")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       )
      ("UTD"
       (mu4e-sent-folder "/home/emiller/.mail/UTD/sent")
       (mu4e-drafts-folder "/UTD/Drafts")
       (user-mail-address "eam150030@utdallas.edu")
       (smtpmail-smtp-user "eam150030@utdallas.edu")
       (smtpmail-local-domain "office365.com")
       (smtpmail-default-smtp-server "smtp.office365.com")
       (smtpmail-smtp-server "smtp.office365.com")
       (smtpmail-stream-type starttls)
       (smtpmail-smtp-service 587)
       )
      ))

  (defun my-mu4e-set-account ()
    "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))
  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
  ;; Include a bookmark to open all of my inboxes
(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/Exchange/INBOX OR maildir:/Gmail/INBOX"
        :key ?i))

;; This allows me to use 'helm' to select mailboxes
(setq mu4e-completing-read-function 'completing-read)
;; Why would I want to leave my message open after I've sent it?
(setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; Don't ask to quit... why is this the default?
(setq mu4e-confirm-quit nil))

;; (def-package! mu4e-alert
;;   :after mu4e
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;     (concat
;;      "flag:unread maildir:/Exchange/INBOX "
;;      "OR "
;;      "flag:unread maildir:/Gmail/INBOX"
;;      ))
;;   ;; (mu4e-alert-enable-mode-line-display)
;;   (defun emiller-refresh-mu4e-alert-mode-line ()
;;     (interactive)
;;     (mu4e~proc-kill)
;;     ;; (mu4e-alert-enable-mode-line-display)
;;     )
;;   (run-with-timer 0 60 'emiller-refresh-mu4e-alert-mode-line)

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
