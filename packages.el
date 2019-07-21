;;; packages.el --- description -*- lexical-binding: t; -*-

;; Private
(package! edit-server)
(package! ivy-yasnippet)
(package! auto-org-md)
(package! org-noter)
(package! org-clock-csv)
(package! easy-hugo)
(package! package-lint)
(package! graphviz-dot-mode)
(package! jest)
(package! vyper-mode)
(package! org-pomodoro)
(package! org-gcal)
(package! dired-sidebar)
;; Experimental
(package! reformatter)
(package! org-super-agenda)
(package! tox)
(package! emms)
(package! hydra-posframe :recipe (:fetcher github :repo "ladicle/hydra-posframe" :files ("*")))
(package! github-review)

(provide 'packages)
;;; packages.el ends here
