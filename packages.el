;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(package! tao-theme)
(package! color-identifiers-mode)

(unpin! doom-themes)

(package! org-roam-server)
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
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode"))
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))

(package! speed-type)

(provide 'packages)
;;; packages.el ends here
