;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(unpin! doom-themes)

(package! edit-server)
;; (package! package-lint)
(package! graphviz-dot-mode)
(package! jest :recipe (:local-repo "~/src/emacs/emacs-jest" :build (:not compile)))
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode" :build (:not compile)))
(package! academic-phrases)
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))
(package! org-chef)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-transclusion)
(package! org-roam-timestamps :recipe (:host github :repo "ThomasFKJorna/org-roam-timestamps"))
(unpin! web-mode)

;; Experimental
(package! webpaste)
(package! mastodon)
(package! gh-notify)
(package! flycheck-vale)
(unpin! pdf-tools)
(package! quarto-mode)
(package! conf-data-toml :recipe (:host github :repo "tecosaur/conf-data-toml"))
(package! shell-maker :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))
(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! speed-type)
;; HACK
(package! nose :disable t)

(provide 'packages)
;;; packages.el ends here
