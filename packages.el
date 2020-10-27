;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(package! tao-theme)
(package! color-identifiers-mode)

(unpin! doom-themes)

(package! org-roam-server :pin "1dc94e102d60e53bb9929b1cdc55d4d8c2b0d64f")
(package! edit-server)
(package! ivy-yasnippet)
(package! auto-org-md)
(package! org-clock-csv)
;; (package! package-lint)
(package! pretty-hydra)
(package! graphviz-dot-mode)
(package! jest)
(package! vyper-mode)
(package! dired-sidebar)
;; Experimental
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode" :no-byte-compile t))
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))

(package! speed-type)

(provide 'packages)
;;; packages.el ends here
