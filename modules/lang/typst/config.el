;;; lang/typst/config.el -*- lexical-binding: t; -*-

;; (use-package! treesit-auto
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package! typst-ts-mode
  :init
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (setq typst-ts-mode-watch-options "--open")

  ;; experimental settings (I'm the main dev, so I enable these)
  (setq typst-ts-mode-enable-raw-blocks-highlight t)
  (setq typst-ts-mode-highlight-raw-blocks-at-startup t))

  ;;   :config
  ;; (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))
