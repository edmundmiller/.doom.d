;;; lang/snakemake/config.el -*- lexical-binding: t; -*-

(use-package! snakemake-mode
  :config
  (after! snakemake-mode
    (set-formatter! 'snakefmt '("snakefmt" "-") :modes '(snakemake-mode))))
