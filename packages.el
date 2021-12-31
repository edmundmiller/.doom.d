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
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-transclusion)

(package! speed-type)
;; HACK
(package! code-review :disable t)
(package! nose :disable t)

(provide 'packages)
;;; packages.el ends here
