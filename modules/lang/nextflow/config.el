;;; lang/nextflow/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :init
  (when (modulep! +lsp)
    (add-hook 'nextflow-mode-hook #'lsp! 'append))
  :config
  (set-formatter! 'nextflow-lint '("nextflow" "lint" "-format") :modes '(nextflow-mode))

  ;; (when (modulep! +lsp)
  ;;   (setq-hook! 'nextflow-mode +format-with 'lsp)
  (setq! lsp-nextflow-formatting-harshil-alignment t)) ;;)
