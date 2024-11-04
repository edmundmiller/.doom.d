;;; lang/nextflow/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :config
  (setq-hook! 'nextflow-mode +format-with 'lsp))
