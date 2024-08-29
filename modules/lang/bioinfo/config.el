;;; lang/bioinfo/config.el -*- lexical-binding: t; -*-

(use-package! nextflow-mode
  :config
  (set-docsets! 'nextflow-mode "Groovy")
  (add-to-list 'lsp-language-id-configuration '(nextflow-mode . "nextflow"))
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("java" "-jar" "~/src/nf-core/language-server/build/libs/language-server-all.jar"))
                        :activation-fn (lsp-activate-on "nextflow")
                        :server-id 'nf-ls)))

(use-package! snakemake-mode
  :config
  (after! snakemake-mode
    (set-formatter! 'snakefmt '("snakefmt" "-") :modes '(snakemake-mode))))
