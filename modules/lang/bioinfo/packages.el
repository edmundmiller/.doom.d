;; -*- no-byte-compile: t; -*-
;;; lang/bioinfo/packages.el

(package! nextflow-mode :recipe (:host github :repo "edmundmiller/nextflow-mode"))
(package! lsp-mode :recipe (:local-repo "~/src/emacs/lsp-mode" :build (:not compile)))
(package! snakemake-mode)
