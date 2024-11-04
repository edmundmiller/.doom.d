;; -*- no-byte-compile: t; -*-
;;; lang/nextflow/packages.el

(package! nextflow-mode :recipe (:host github :repo "edmundmiller/nextflow-mode"))
;; TODO PR branch up-stream
(package! lsp-mode :recipe (:local-repo "~/src/emacs/lsp-mode" :build (:not compile)))
