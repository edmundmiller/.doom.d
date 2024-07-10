;;; lang/bioinfo/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :config
  (set-docsets! 'nextflow-mode "Groovy"))

(use-package! snakemake-mode
  :config
  (set-formatter! 'snakefmt '("snakefmt" "-") :modes '(snakemake-mode)))
