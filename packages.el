;;; packages.el --- description -*- lexical-binding: t; -*-

(package! solidity-mode)
(when (featurep! :completion company)
  (package! company-solidity))

;; Private
(package! edit-server)
(package! org-gcal)
;; (package! exec-path-from-shell)
(package! floobits)
;; (package! with-editor)
;; Experimental
(package! libmpdel)
(package! mpdel)

(provide 'packages)
;;; packages.el ends here
