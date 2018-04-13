;;; packages.el --- description -*- lexical-binding: t; -*-

(package! solidity-mode)
(when (featurep! :completion company)
  (package! company-solidity))

;; Private
(package! edit-server)
(package! org-gcal)
(package! floobits)
;; Experimental
;; (package! all-the-icons-dired)
;; (package! mu4e-alert)
(package! company-box)
(package! docker)
(package! libmpdel)
(package! mpdel)

(provide 'packages)
;;; packages.el ends here
