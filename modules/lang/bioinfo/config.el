;;; lang/bioinfo/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :config
  (setq-hook! 'nextflow-mode +format-with 'lsp))

(use-package! snakemake-mode
  :config
  (after! snakemake-mode
    (set-formatter! 'snakefmt '("snakefmt" "-") :modes '(snakemake-mode))))
