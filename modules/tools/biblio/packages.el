;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :lang org)
  (package! org-ref :pin "120509c38929cc25d814e9a42092c44cb34ec34e"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex))
