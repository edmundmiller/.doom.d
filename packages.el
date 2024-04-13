;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(unpin! doom-themes)

;; (package! package-lint)
(package! graphviz-dot-mode)
(package! jest :recipe (:local-repo "~/src/emacs/emacs-jest" :build (:not compile)))
(package! nextflow-mode :recipe (:local-repo "~/src/emacs/nextflow-mode" :build (:not compile)))
(package! ob-duckdb :recipe (:local-repo "~/src/emacs/ob-duckdb" :build (:not compile)))
(package! academic-phrases)
(package! mu4e-conversation)
(package! mu4e-patch :recipe (:host github :repo "seanfarley/mu4e-patch"))
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-transclusion)
(package! org-roam-timestamps :recipe (:host github :repo "ThomasFKJorna/org-roam-timestamps"))
(package! snakemake-mode)
(unpin! org-journal)

;; Experimental
(unpin! parinfer-rust-mode)
(package! just-mode)
(package! justl)
(package! auth-source-1password :recipe (:host github :repo "dlobraico/auth-source-1password"))
(package! tldr)
(package! sqlformat)
(package! age)
(package! difftastic)
(unpin! apheleia)
(package! nushell-ts-mode :recipe (:host github :repo "herbertjones/nushell-ts-mode"))
(package! nushell-ts-babel :recipe (:host github :repo "herbertjones/nushell-ts-babel"))
(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh"))
(unpin! evil-collection)
(package! agenix :recipe (:host github :repo "t4ccer/agenix.el"))
;; FIXME not working yet
(package! ob-julia :recipe (:host github :repo "karthink/ob-julia"))
(package! engrave-faces)
(package! ox-chameleon :recipe (:host nil :repo "https://git.tecosaur.net/tec/ox-chameleon"))
(package! webpaste)
(package! mastodon)
(package! gh-notify)
(unpin! pdf-tools)
(package! conf-data-toml :recipe (:host github :repo "tecosaur/conf-data-toml"))
(package! julia-formatter
  :recipe (:host codeberg :repo "FelipeLema/julia-formatter.el"
           :files ( "julia-formatter.el" ;; main script executed by Emacs
                    "formatter_service.jl" ;; script executed by Julia
                    "Manifest.toml" "Project.toml"))) ;; project files

(package! elfeed-tube)
(package! elfeed-tube-mpv)

(provide 'packages)
;;; packages.el ends here
