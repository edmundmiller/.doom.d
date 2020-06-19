;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "8a0dd9841316793aacddea744d6b8ca4a7857a35"))
(when (featurep! :lang org)
  (package! org-ref :pin "4ce80644377f2369efb475bd58a57cf6950d8c41"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "389b2578562690e7b3fec7d43abddee5b3e85fa4"))
