;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +cape-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-line' or `cape-dabbrev'.
As an exception, `cape-line' will also scan buffers with the same
major mode regardless of size.")

;;
;;; Packages
(use-package! corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :hook (org-mode . corfu-mode)
  :init
  ;; Auto-completion settings, must be set before calling `global-corfu-mode'.
  ;; Due to lazy-loading, overriding these in config.el works too.
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-separator ?\s
        corfu-excluded-modes '(erc-mode
                               circe-mode
                               help-mode
                               gud-mode
                               vterm-mode))
  ;; `:g' is needed here to prevent `global-map' from overriding this with
  ;; `set-mark-command'.
  (map! :unless (modulep! +tng) :gi "C-SPC" #'completion-at-point)
  :config
  (setq corfu-cycle t
        corfu-preselect (if (modulep! :completion corfu +tng) 'prompt t)
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary t
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))

  (defun corfu-disable-in-minibuffer-p ()
    (or (bound-and-true-p mct--active)
        (bound-and-true-p vertico--input)
        (and (featurep 'helm-core) (helm--alive-p))
        (eq (current-local-map) read-passwd-map)))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (unless (corfu-disable-in-minibuffer-p)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode +1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (defun corfu-visible-p ()
    (or (and (frame-live-p corfu--frame)
             (frame-visible-p corfu--frame))
        (and (featurep 'corfu-terminal)
             (popon-live-p corfu-terminal--popon))))

  ;; If you want to update the visual hints after completing minibuffer commands
  ;; with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (corfu-visible-p)
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        ;; Pending a PR I (@LemonBreezes) am making to expose `anzu--update-timer'.
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  ;; Allow completion after `:' in Lispy.
  (add-to-list 'corfu-auto-commands #'lispy-colon)

  (when (modulep! +orderless)
    (after! orderless
      (setq orderless-component-separator #'orderless-escapable-split-on-space)))

  (add-hook! 'evil-insert-state-exit-hook
    (defun +corfu-quit-on-evil-insert-state-exit-h ()
      ;; This predicate a workaround for unexpected calls to `corfu-quit' in
      ;; :company-doc-buffer buffers. This was specifically happening when using
      ;; `yasnippet-capf' and `company-yasnippet'.
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (corfu-quit))))

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (defun +corfu--reset-or-passthrough (cmd)
    (when (and (modulep! +tng)
               (> corfu--index -1)
               (eq corfu-preview-current 'insert))
      cmd))
  (defun +corfu--backward-toggle-escape-sep (cmd)
    (save-excursion
      (backward-char 1)
      (if (looking-back "\\\\" -1)
          #'corfu-reset
        (lambda ()
          (interactive)
          (save-excursion
            (backward-char 1)
            (insert-char ?\\))))))
  (defun +corfu--insert-separator-or-toggle-escape (cmd)
    (if (char-equal (char-before) corfu-separator)
        (+corfu--backward-toggle-escape-sep cmd)
      cmd))
  (let ((mi-del '(menu-item "corfu-reset-or-passthrough" corfu-reset
                  :filter +corfu--maybe-reset-backspace-filter))
        (mi-c-spc '(menu-item "corfu-insert-separator-or-toggle-escape" corfu-insert-separator
                    :filter +corfu--insert-separator-or-toggle-escape)))
    (map! :map corfu-map
          [return] #'corfu-insert
          "RET" #'corfu-insert
          (:when (modulep! +orderless)
            :gi "C-SPC" mi-c-spc)
          (:when (modulep! +tng)
            [tab] #'corfu-next
            [backtab] #'corfu-previous
            "TAB" #'corfu-next
            "S-TAB" #'corfu-previous
            [backspace] mi-del
            "DEL" mi-del)))

  (after! vertico
    (map! :map corfu-map
          "M-m" #'corfu-move-to-minibuffer
          (:when (modulep! :editor evil) "M-J" #'corfu-move-to-minibuffer))))

(use-package! cape
  :commands
  cape-abbrev
  cape-dabbrev
  cape-dict
  cape-elisp-block
  cape-elisp-symbol
  cape-emoji
  cape-file
  cape-history
  cape-keyword
  cape-line
  cape-rfc1345
  cape-sgml
  cape-tex
  cape-company-to-capf
  cape-capf-super
  cape-capf-buster
  cape-capf-accept-all
  cape-capf-debug
  cape-capf-silent
  cape-capf-purify
  cape-capf-nonexclusive
  cape-capf-noninterruptable
  cape-capf-properties
  cape-capf-predicate
  cape-capf-prefix-length
  cape-capf-inside-comment
  cape-capf-inside-string
  cape-capf-inside-faces
  cape-capf-interactive
  cape-wrap-buster
  cape-wrap-accept-all
  cape-wrap-debug
  cape-wrap-silent
  cape-wrap-purify
  cape-wrap-nonexclusive
  cape-wrap-noninterruptable
  cape-wrap-properties
  cape-wrap-predicate
  cape-wrap-prefix-length
  cape-wrap-inside-comment
  cape-wrap-inside-string
  cape-wrap-inside-faces
  cape-interactive
  :init
  (add-hook! prog-mode
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (add-hook! (org-mode markdown-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (when (modulep! +dabbrev)
    ;; Set up `cape-dabbrev' options.
    (defun +dabbrev-friend-buffer-p (other-buffer)
      (< (buffer-size other-buffer) +cape-buffer-scanning-size-limit))
    (setq cape-dabbrev-check-other-buffers t
          dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
          dabbrev-ignored-buffer-regexps
          '("\\.\\(?:pdf\\|jpe?g\\|png\\|svg\\|eps\\)\\'"
            "^ "
            "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
          dabbrev-upcase-means-case-search t)
    (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
                          eshell-mode)
      (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))
  (when (modulep! +line)
    ;; Set up `cape-line' options.
    (defun +cape-line-buffers ()
      (cl-loop for buf in (buffer-list)
               if (or (eq major-mode (buffer-local-value 'major-mode buf))
                      (< (buffer-size buf) +cape-buffer-scanning-size-limit))
               collect buf))
    (setq cape-line-buffer-function #'+cape-line-buffers)
    (add-hook! (text-mode comint-mode minibuffer-setup)
      (add-hook 'completion-at-point-functions #'cape-line 20 t)))

  ;; Complete emojis :).
  (when (and (modulep! +emoji) (> emacs-major-version 28))
    (add-hook! (prog-mode conf-mode)
      (add-hook 'completion-at-point-functions
                (cape-capf-inside-faces
                 (cape-capf-prefix-length #'cape-emoji 1)
                 ;; Only call inside comments and docstrings.
                 'tree-sitter-hl-face:doc 'font-lock-doc-face
                 'font-lock-comment-face 'tree-sitter-hl-face:comment)
                10 t))
    (add-hook! text-mode
      (add-hook 'completion-at-point-functions
                (cape-capf-prefix-length #'cape-emoji 1) 10 t)))

  ;; Enable dictionary-based autocompletion.
  (when (modulep! +dict)
    (add-hook! text-mode
      (add-hook 'completion-at-point-functions #'cape-dict 40 t))
    (add-hook! (prog-mode conf-mode)
      (add-hook 'completion-at-point-functions
                (cape-capf-inside-faces
                 ;; Only call inside comments and docstrings.
                 #'cape-dict 'tree-sitter-hl-face:doc 'font-lock-doc-face
                 'font-lock-comment-face 'tree-sitter-hl-face:comment)
                40 t)))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :defer t
  :init
  (add-hook! yas-minor-mode
    (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t)))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook (corfu-mode . corfu-terminal-mode))

;;
;;; Extensions

(use-package! corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package! corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! :map corfu-map
        "C-<up>" #'corfu-popupinfo-scroll-down
        "C-<down>" #'corfu-popupinfo-scroll-up
        "C-S-p" #'corfu-popupinfo-scroll-down
        "C-S-n" #'corfu-popupinfo-scroll-up
        "C-h" #'corfu-popupinfo-toggle)
  (map! :when (modulep! :editor evil)
        :map corfu-popupinfo-map
        ;; Reversed because popupinfo assumes opposite of what feels intuitive
        ;; with evil.
        "C-S-k" #'corfu-popupinfo-scroll-down
        "C-S-j" #'corfu-popupinfo-scroll-up))