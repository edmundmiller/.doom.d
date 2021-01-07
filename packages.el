;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(package! tao-theme)
(package! color-identifiers-mode)

(unpin! doom-themes)

(package! org-roam-server :pin "1dc94e102d60e53bb9929b1cdc55d4d8c2b0d64f")
(package! edit-server)
;; (package! package-lint)
(package! graphviz-dot-mode)
(package! jest)
;; Experimental
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode" :no-byte-compile t))
(package! org-roam-sbl-show-broken-links :recipe (:host github :repo "twitchy-ears/org-roam-sbl-show-broken-links" :branch "main"))
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))
(package! org-chef)

(package! speed-type)

(provide 'packages)
;;; packages.el ends here
