;;; lang/bioinfo/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :config
  (set-docsets! 'nextflow-mode "Groovy"))

(use-package! snakemake-mode
  :config
  (after! snakemake-mode
    (set-formatter! 'snakefmt '("snakefmt" "-") :modes '(snakemake-mode))))
