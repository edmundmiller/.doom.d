;;; config-new.el -*- lexical-binding: t; -*-

(setq user-mail-address "Edmund.A.Miller@gmail.com"
      user-full-name "Edmund Miller"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      ;; doom-theme 'doom-one
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; This used to be off by default
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil

      ;; More common use-case
      evil-ex-substitute-global t)

(use-package! tao-theme
  :init
  (setq tao-theme-sepia-depth 3.38
        tao-theme-sepia-saturation 0.98)
  :config
  (load-theme 'tao-yang t)
  (custom-set-faces!
    '(default :background "#f7f3ee" :foreground "#605a52")
    '(+workspace-tab-selected-face :background "#67665F" :foreground "#FFFFF5")
    '(+org-todo-active :background "#67665F" :foreground "#FFFFF5")
    '(org-roam-link :foreground "blue")
    '(org-roam-link-invalid :foreground "red")))

(use-package! color-identifiers-mode
  :hook prog-mode)

;;
;;; UI

;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;
;;; Keybinds

(map!
 :n [tab] (cmds! (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 #'+fold/toggle
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
 :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
 (:leader
  (:prefix "f"
   :desc "Find file in dotfiles" :n "o" #'+emiller/find-in-dotfiles
   :desc "Browse dotfiles" :n "O" #'+emiller/browse-dotfiles)
  (:prefix "i"
   :desc "Insert date" :n "d" #'insert-todays-date)
  (:prefix "n"
   (:prefix "j"
    :desc "New Date Entry" :n "n" #'org-journal-new-date-entry))
  (:prefix "o"
   :desc "Calc" :n "c" #'calc
   :desc "easy-hugo" :n "g" #'easy-hugo
   :desc "APP: IRC" :n "i" #'=irc
   ;; :desc "APP: notmuch" :n "m" #'=mu4e
   ;; :desc "dired-sidebar" :n "n" #'dired-sidebar-toggle-sidebar
   :desc "todo.org" :n "o" #'+emiller/visit-todo-org
   :desc "projects" :n "p" #'+emiller/visit-projects-org
   :desc "emms" :n "s" #'emms
   :desc "APP: rss" :n "," #'=rss)))

;;
;;; Modules

;;; :completion ivy
(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :editor evil
(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote))

;;; :emacs dired
;;Get rid of dired message when using "a"
(put 'dired-find-alternate-file 'disabled nil)

;;; :ui doom-dashboard
;; NARF
(setq fancy-splash-image (concat doom-private-dir "narf.png"))
;; Don't need the menu; I know them all by heart
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;; :checks grammar
(after! langtool
  (setq langtool-bin "languagetool-commandline"))

;;; :tools biblio
(setq! +biblio-pdf-library-dir "~/sync/papers"
       +biblio-default-bibliography-files '("~/sync/reference/bibliography.bib"
                                            "~/sync/reference/molecular_biology.bib"
                                            "~/sync/reference/molecular_biology_project.bib"
                                            "~/sync/reference/books.bib")
       +biblio-notes-path "~/sync/org/roam/bib/")

;;; :tools direnv
(setq direnv-always-show-summary nil)

;;; :tools magit
(setq magit-repository-directories '(("~/src" . 3))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=BD387FF7BC10AA9D")
                         (magit-rebase "--autosquash" "--gpg-sign=BD387FF7BC10AA9D")
                         (magit-pull "--rebase" "--gpg-sign=BD387FF7BC10AA9D")))

;; Enable git gutter on tramp sessions
(defun +version-control|git-gutter-maybe ()
  (when buffer-file-name
    (require 'git-gutter-fringe)
    (git-gutter-mode +1)))

(setenv "EDITOR" "emacsclient")

;;; :lang clojure
(after! cider
  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :quit nil)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil))))

;;; :lang ledger
(add-to-list 'auto-mode-alist '("\\.\\(h?ledger\\|journal\\|j\\)$" . ledger-mode))

;;; :lang org
(setq org-directory "~/sync/org/"
      org-archive-location (concat org-directory "archive/%s::")
      org-roam-directory (concat org-directory "roam/")
      org-startup-folded 'overview
      org-ellipsis " [...] "
      org-export-with-toc nil
      org-log-done 'time
      +org-roam-open-buffer-on-find-file nil
      org-attach-id-dir (concat org-roam-directory "data/")
      ;; Fix org-id on SPC-l-s
      ;; org-id-link-to-org-use-id 'use-existing
      org-deadline-warning-days 5)

(after! org-roam
  (setq org-roam-db-location (concat org-roam-directory ".org-roam.db")))

(defvar org-contacts-files '("~/sync/org/contacts.org"))

(after! org
  (setq org-capture-templates
        (append
         ;; TODO generalize these with org-directory
         '(("a" "Appointment" entry (file  "~/sync/org/schedule.org")
            "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
           ("e" "Lab Entry" entry
            (file+olp+datetree "~/sync/org/roam/lab/2020.org")
            "* %U %?\n%i\n%a")
           ("l" "Link" entry (file+headline "~/sync/org/links.org" "Links")
            "* %a %^g\n %?\n %i" :immediate-finish t))
         org-capture-templates)))



;; To make `org-latex-preview` work
(after! org
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t
           ("pdflatex"))
          ("T1" "fontenc" t
           ("pdflatex"))
          ("" "graphicx" t)
          ("" "grffile" nil)
          ("" "longtable" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("" "hyperref" nil))))


;; I like to cross things off my todo list
(custom-set-faces! '(org-headline-done :strike-through t))

;; Start in insert mode in org-capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; +journal
(after! org
  (setq org-journal-dir (concat org-roam-directory "journal/")
        org-journal-encrypt-journal t
        org-journal-file-format "%Y%m%d.org"))

;; +noter
(after! org-noter
  (setq org-noter-always-create-frame t
        org-noter-doc-split-fraction '(0.65 . 0.35)
        org-noter-separate-notes-from-heading t
        org-noter-default-heading-title "Page $p$"
        org-noter-auto-save-last-location t
        org-noter-notes-search-path '("~/sync/org/roam/lit/")
        org-noter-separate-notes-from-heading t
        org-noter-doc-property-in-notes t))

;; +roam
(after! org-roam
  (setq org-roam-tag-sources '(prop all-directories)
        org-roam-buffer-width 0.25)
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+roam_key: ${ref}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t))))

(use-package! org-roam-sbl-show-broken-links
  :after org-roam)

(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-default-exclude-filters (json-encode (list (list (cons 'id "journal") (cons 'parent "tags"))))
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(after! org
  (setq org-beamer-theme "[progressbar=foot]metropolis"
        org-beamer-frame-level 2))


;;; :lang python
(after! lsp-python-ms
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (set-lsp-priority! 'mspyls 1))
(set-eglot-client! 'python-mode (list (executable-find "python-language-server")))

;;; :lang rust
(setq rustic-lsp-server 'rust-analyzer)

;;; :lang solidity
(setq flycheck-solidity-solium-soliumrcfile "/home/emiller/sync/.soliumrc.json"
      solidity-flycheck-solc-checker-active t)

;;; :lang web
;; Hook into vue-lsp
(add-hook 'web-mode-local-vars-hook #'lsp!)

;;; :email mu4e
;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(after! mu4e
  (setq shr-use-colors nil)
  (set-email-account! "Gmail"
                      '((mu4e-sent-folder       . "/Gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder     . "/Gmail/[Gmail]/Drafts")
                        (mu4e-trash-folder      . "/Gmail/[Gmail]/Trash")
                        (mu4e-refile-folder     . "/Gmail/[Gmail]/Archive")
                        (smtpmail-smtp-user     . "edmund.a.miller@gmail.com")
                        (mu4e-compose-signature . "---\nEdmund Miller"))
                      t)

  (set-email-account! "Eman"
                      '((mu4e-sent-folder       . "/Eman/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder     . "/Eman/[Gmail]/Drafts")
                        (mu4e-trash-folder      . "/Eman/[Gmail]/Trash")
                        (mu4e-refile-folder     . "/Eman/[Gmail]/Archive")
                        (mu4e-compose-signature . "---\nEdmund Miller"))
                      t)

  (set-email-account! "UTD"
                      '((mu4e-sent-folder       . "/UTD/Sent")
                        (mu4e-drafts-folder     . "/UTD/Drafts")
                        (mu4e-trash-folder      . "/UTD/Trash")
                        (mu4e-refile-folder     . "/UTD/Archive")
                        (smtpmail-smtp-user     . "Edmund.Miller@utdallas.edu")
                        (mu4e-compose-signature . "---\nEdmund Miller"))
                      t))

(use-package! mu4e-patch
  :hook (mu4e-view-mode . mu4e-patch-highlight))

;;; :app calendar
(use-package! org-gcal
  :config
  (setq org-gcal-client-id "119671856150-j6j4b8hjm1k8d1v2ar39c2g1ifdv8iq9.apps.googleusercontent.com"
        org-gcal-client-secret "KOa_aQ-SsyNkK_K4Y5ePk-k1"
        ;; TODO Generalize
        org-gcal-file-alist '(("Edmund.a.miller@gmail.com" .  "~/sync/org/schedule.org"))))

;;; :app irc
(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "emiller88"
                     :sasl-username ,(+pass-get-user "irc/freenode.net")
                     :sasl-password (lambda (&rest _) (+pass-get-secret "irc/freenode.net"))
                     :channels ("#bioinformatics" "#clojure" "#emacs" "#emacs-circe" "#guix" "#guile" "#home-manager" "#nixos" "#nixos-emacs" "#sway" "##rust" "#python" "#pine64"))))
;;; :app rss
(after! elfeed-search
  (map! :map elfeed-search-mode-map
        :localleader
        :n "m" #'my/elfeed-search-view-hydra/body
        :n "s" #'elfeed-toggle-star
        :n "v" #'elfeed-view-mpv
        :n "r" #'elfeed-update))
;; Set max width
(after! elfeed
  (setq elfeed-search-title-max-width 120))


;;
;;; Language customizations

(custom-theme-set-faces! 'doom-dracula
  `(markdown-code-face :background ,(doom-darken 'bg 0.075))
  `(font-lock-variable-name-face :foreground ,(doom-lighten 'magenta 0.6)))


;;
;;; Packages

(use-package! graphviz-dot-mode)
(use-package! jest)
(use-package! nextflow-mode)

;;
;;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((ssh-deploy-async-with-threads . 1)
     (ssh-deploy-on-explicity-save . t)
     (ssh-deploy-async . 1))))
