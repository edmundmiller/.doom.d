;;; lang/typst/config.el -*- lexical-binding: t; -*-

(use-package! typst-ts-mode
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")

  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t)

  :config
  (let ((typst-recipe (make-treesit-auto-recipe
                       :lang 'typst
                       :ts-mode 'typst-ts-mode
                       :url "https://github.com/uben0/tree-sitter-typst"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list typst-recipe)))
