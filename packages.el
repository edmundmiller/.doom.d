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
(package! snakemake-mode)
(unpin! web-mode)
(unpin! org-journal)

;; Experimental
(package! agenix :recipe (:host github :repo "t4ccer/agenix.el"))
(package! whisper :recipe (:host github :repo "natrys/whisper.el" :files ("*.el")))
(package! engrave-faces)
(package! ox-chameleon :recipe (:host nil :repo "https://git.tecosaur.net/tec/ox-chameleon"))
(package! webpaste)
(package! mastodon)
(package! gh-notify)
(package! flycheck-vale)
(unpin! pdf-tools)
(package! quarto-mode)
(package! conf-data-toml :recipe (:host github :repo "tecosaur/conf-data-toml"))
(package! chatgpt-shell)
(package! ob-chatgpt-shell)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! julia-formatter
  :recipe (:host codeberg :repo "FelipeLema/julia-formatter.el"
           :files ( "julia-formatter.el" ;; main script executed by Emacs
                    "formatter_service.jl" ;; script executed by Julia
                    "Manifest.toml" "Project.toml"))) ;; project files

(package! elfeed-tube)
(package! elfeed-tube-mpv)

(package! speed-type)
;; HACK
(package! nose :disable t)
(package! closql :pin "0a7226331ff1f96142199915c0ac7940bac4afdd")

(provide 'packages)
;;; packages.el ends here
