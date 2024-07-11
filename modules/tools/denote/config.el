;;; tools/denote/config.el -*- lexical-binding: t; -*-

(use-package! denote
  :config
  (require 'denote)

  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/sync/org/denote"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "nix" "bioinformatics" "genomics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)


  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.


  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/sync/papers")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)


  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (let ((map global-map))
    (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-add-links)
    (define-key map (kbd "C-c n b") #'denote-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-find-link)
    (define-key map (kbd "C-c n f b") #'denote-find-backlink)
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
    (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
    (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.


  ;; If you want to have Denote commands available via a right click
  ;; context menu, use the following and then enable
  ;; `context-menu-mode'.
  (add-hook 'context-menu-functions #'denote-context-menu)

  (map!
   :leader
   ;; HACK https://github.com/doomemacs/doomemacs/pull/7935
   "n d" nil
   (:prefix "n"
            (:prefix ("d" . "denote")
             :desc "Toggle denote buffer"         "d" #'denote
             ;; :desc "Open random node"           "a" #'org-roam-node-random
             ;; :desc "Find node"                  "f" #'org-roam-node-find
             ;; :desc "Find ref"                   "F" #'org-roam-ref-find
             ;; :desc "Show graph"                 "g" #'org-roam-graph
             ;; :desc "Insert node"                "i" #'org-roam-node-insert
             ;; :desc "Capture to node"            "n" #'org-roam-capture
             ;; :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
             ;; :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
             ;; :desc "Sync database"              "s" #'org-roam-db-sync
             ;; (:prefix ("d" . "by date")
             ;;  :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
             ;;  :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
             ;;  :desc "Capture date"              "D" #'org-roam-dailies-capture-date
             ;;  :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
             ;;  :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
             ;;  :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
             ;;  :desc "Capture today"             "n" #'org-roam-dailies-capture-today
             ;;  :desc "Goto today"                "t" #'org-roam-dailies-goto-today
             ;;  :desc "Capture today"             "T" #'org-roam-dailies-capture-today
             ;;  :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
             ;;  :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
             ;;  :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))))
             ))))
