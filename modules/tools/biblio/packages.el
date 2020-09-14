;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :lang org)
  (package! org-ref :pin "3f9d9fa096b97d81981bec6cc70b791b56e49f20"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "a9a7d232ce25d06880aa2ed16148615af7e551a7"))
