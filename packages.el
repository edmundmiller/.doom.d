;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(unpin! doom-themes)

(package! edit-server)
;; (package! package-lint)
(package! graphviz-dot-mode)
(package! jest)
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode" :build (:not compile)))
;; Experimental
(package! academic-phrases)
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))
(package! org-chef)

(package! speed-type)

(provide 'packages)
;;; packages.el ends here
